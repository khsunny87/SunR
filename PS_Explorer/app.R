options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(DT)
  library(MatchIt)
  library(cobalt)
  library(smd)
  library(dplyr)
  library(purrr)
  library(readr)
  library(ggplot2)
})

# ── Helpers ───────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

get_binary_candidates <- function(df) {
  if (is.null(df)) return(character(0))
  names(Filter(function(x) dplyr::n_distinct(x, na.rm = TRUE) == 2, df))
}

# Variable Selection용: Shapiro-Wilk 기반 mean±SD 또는 median[IQR]
fmt_stat <- function(x, is_normal = NULL) {
  x <- na.omit(x)
  if (length(x) == 0) return("")
  if (is.numeric(x)) {
    if (is.null(is_normal)) {
      n <- length(x)
      is_normal <- if (n >= 3 && n <= 5000) {
        tryCatch(shapiro.test(x)$p.value > 0.05, error = function(e) FALSE)
      } else FALSE
    }
    if (is_normal) sprintf("%.2f ± %.2f", mean(x), sd(x))
    else {
      q <- quantile(x, c(0.25, 0.5, 0.75))
      sprintf("%.1f [%.1f–%.1f]", q[2], q[1], q[3])
    }
  } else {
    tab <- table(x); n_t <- sum(tab)
    paste(paste0(names(tab), ": ", tab, " (", round(tab / n_t * 100, 1), "%)"),
          collapse = "; ")
  }
}

# Balance Table용: 가중/비가중 mean±SD 또는 n(%)
fmt_stat_w <- function(x, w = NULL, is_cont) {
  if (is.null(w)) w <- rep(1, length(x))
  valid <- !is.na(x) & !is.na(w) & w > 0
  x <- x[valid]; w <- w[valid]
  if (length(x) == 0) return("")
  if (is_cont) {
    wm  <- weighted.mean(x, w)
    wsd <- sqrt(sum(w * (x - wm)^2) / sum(w))
    sprintf("%.1f ± %.1f", wm, wsd)
  } else {
    ux <- as.character(sort(unique(x)))
    tab_n <- table(x)
    tab_p <- sapply(ux, function(lv) sum(w[as.character(x) == lv]) / sum(w) * 100)
    paste(paste0(ux, ": ", tab_n[ux], " (", round(tab_p, 1), "%)"), collapse = "; ")
  }
}

# Variable Selection 테이블 계산
compute_var_table <- function(df, grp_var) {
  covs       <- setdiff(names(df), grp_var)
  grp_vals   <- df[[grp_var]]
  grp_levels <- as.character(sort(unique(na.omit(grp_vals))))

  purrr::map_dfr(covs, function(v) {
    x         <- df[[v]]
    is_cont   <- is.numeric(x)
    n_overall <- sum(!is.na(x))
    is_normal <- if (is_cont) {
      x_clean <- na.omit(x); n <- length(x_clean)
      if (n >= 3 && n <= 5000) tryCatch(shapiro.test(x_clean)$p.value > 0.05, error = function(e) FALSE)
      else FALSE
    } else NULL

    overall_val <- fmt_stat(x, is_normal)
    grp_stats   <- setNames(
      lapply(grp_levels, function(gl) fmt_stat(x[!is.na(grp_vals) & as.character(grp_vals) == gl], is_normal)),
      grp_levels
    )
    pval <- tryCatch({
      complete <- !is.na(x) & !is.na(grp_vals)
      xc <- x[complete]; gc <- grp_vals[complete]
      if (length(unique(gc)) < 2) return(NA_real_)
      if (is_cont) {
        if (isTRUE(is_normal)) t.test(xc ~ gc)$p.value
        else wilcox.test(xc ~ gc, exact = FALSE)$p.value
      } else {
        ct <- table(xc, gc)
        if (min(ct) < 5) fisher.test(ct, simulate.p.value = TRUE)$p.value
        else chisq.test(ct)$p.value
      }
    }, error = function(e) NA_real_)

    smd_val <- tryCatch(abs(smd::smd(x = x, g = grp_vals, na.rm = TRUE)$estimate),
                        error = function(e) NA_real_)

    row <- tibble::tibble(var = v, type = if (is_cont) "Continuous" else "Categorical",
                          N = n_overall, overall = overall_val, pval = pval, smd = smd_val)
    for (gl in grp_levels) row[[paste0("grp_", gl)]] <- grp_stats[[gl]]
    row
  })
}

# Balance Table용: 선택 공변량만, 가중 통계 포함
compute_balance_stats <- function(df, grp_var, covs, weights = NULL) {
  grp_vals   <- df[[grp_var]]
  grp_levels <- as.character(sort(unique(na.omit(grp_vals))))
  w_all <- if (is.null(weights)) rep(1, nrow(df)) else weights

  purrr::map_dfr(covs, function(v) {
    x       <- df[[v]]
    is_cont <- is.numeric(x)
    complete <- !is.na(x) & !is.na(grp_vals)
    xc <- x[complete]; gc <- as.character(grp_vals[complete]); wc <- w_all[complete]

    overall_val <- fmt_stat_w(xc, wc, is_cont)
    grp_stats <- setNames(
      lapply(grp_levels, function(gl) fmt_stat_w(xc[gc == gl], wc[gc == gl], is_cont)),
      grp_levels
    )
    smd_val <- tryCatch({
      if (is.null(weights)) abs(smd::smd(x = xc, g = factor(gc), na.rm = FALSE)$estimate)
      else                   abs(smd::smd(x = xc, g = factor(gc), w = wc, na.rm = FALSE)$estimate)
    }, error = function(e) NA_real_)

    row <- tibble::tibble(var = v, type = if (is_cont) "Cont." else "Cat.",
                          overall = overall_val, smd = smd_val)
    for (gl in grp_levels) row[[paste0("grp_", gl)]] <- grp_stats[[gl]]
    row
  })
}

# IPTW 가중치 계산
compute_weights <- function(matchit_obj, estimand = "ATT") {
  ps      <- matchit_obj$distance
  trt_var <- as.character(formula(matchit_obj)[[2]])
  trt     <- as.integer(as.character(matchit_obj$model$data[[trt_var]]))
  switch(estimand,
    ATT = ifelse(trt == 1, 1,      ps / (1 - ps)),
    ATE = ifelse(trt == 1, 1 / ps, 1 / (1 - ps)),
    ATC = ifelse(trt == 0, 1,      (1 - ps) / ps)
  )
}

# DT 행 강조 (|SMD| > 0.1 → 연노랑)
highlight_imbal_rows <- function(dt_widget, flag_col = ".__flag__") {
  DT::formatStyle(
    dt_widget, flag_col, target = "row",
    backgroundColor = DT::styleEqual(c("ok", "imbal"), c(NA, "lightyellow")),
    fontWeight      = DT::styleEqual(c("ok", "imbal"), c("normal", "bold"))
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────

sidebar <- sidebarPanel(
  width = 3,
  h4("1) 데이터"),
  fileInput("file", "CSV 업로드 (UTF-8)", accept = c(".csv", "text/csv")),
  prettySwitch("use_example", "예제 데이터 사용", value = TRUE, status = "info"),
  conditionalPanel(
    condition = "input.use_example == true",
    helpText("예제: MatchIt::lalonde (직업훈련 프로그램, N=614)")
  ),
  hr(),
  h4("2) 그룹변수"),
  uiOutput("ui_grp"),
  helpText("0/1 또는 2-레벨 변수만 표시됩니다.")
)

main <- mainPanel(
  width = 9,
  tags$style(HTML("
    table.dataTable td.thick-left, table.dataTable th.thick-left {
      border-left: 3px solid #555 !important;
    }
    table.dataTable td.bg-pre  { background-color: #f5f5f5 !important; }
    table.dataTable td.bg-post { background-color: #e8f4ff !important; }
  ")),
  tabsetPanel(
    id = "tabs",

    tabPanel(
      "Data",
      h4("Preview"),
      textOutput("data_count"),
      tags$hr(),
      DTOutput("data_preview")
    ),

    tabPanel(
      "Variable Selection",
      br(),
      helpText("PS 모델에 포함할 공변량을 선택하세요. |SMD| > 0.1인 행은 강조됩니다."),
      DTOutput("var_table")
    ),

    tabPanel(
      "Method",
      br(),
      h4("분석 방법"),
      prettyRadioButtons(
        "ps_method", NULL,
        choices  = c("Matching" = "matching", "IPTW" = "iptw"),
        selected = "matching", inline = TRUE, status = "primary"
      ),
      hr(),
      conditionalPanel(
        condition = "input.ps_method == 'matching'",
        h5("Matching 옵션"),
        selectInput("match_method", "Method",
                    choices = c("nearest", "optimal", "full"), selected = "nearest"),
        numericInput("ratio",   "Ratio (k:1)", value = 1, min = 1, max = 10, step = 1),
        numericInput("caliper", "Caliper (SD 단위, 0 = 없음)", value = 0.2, min = 0, step = 0.05),
        selectInput("distance", "Distance metric",
                    choices = c("logit", "probit", "linear.logit", "gam"), selected = "logit")
      ),
      conditionalPanel(
        condition = "input.ps_method == 'iptw'",
        h5("IPTW 옵션"),
        prettyRadioButtons("estimand", "Estimand",
                           choices = c("ATT", "ATE", "ATC"),
                           selected = "ATT", inline = TRUE, status = "info"),
        selectInput("distance_iptw", "Distance metric",
                    choices = c("logit", "probit", "linear.logit", "gam"), selected = "logit")
      ),
      hr(),
      prettySwitch("drop_na_ps", "Complete-case (결측 행 제거)",
                   value = TRUE, status = "warning"),
      actionButton("run_ps", "Run Analysis", class = "btn-primary btn-lg", icon = icon("play"))
    ),

    tabPanel(
      "Results",
      br(),
      uiOutput("result_info"),
      br(),
      tabsetPanel(
        id = "result_tabs",
        tabPanel("Love Plot",    br(), plotOutput("love_plot", height = "500px")),
        tabPanel("Balance Plot", br(),
                 selectInput("bal_var", "변수 선택", choices = NULL),
                 plotOutput("bal_plot", height = "420px")),
        tabPanel("Balance Table", br(), DTOutput("bal_tbl"))
      )
    )
  )
)

ui <- fluidPage(
  titlePanel("PS Explorer — Propensity Score Analysis"),
  sidebarLayout(sidebar, main)
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── 데이터 로딩 ──────────────────────────────────────────────────────────────
  data_raw <- reactive({
    if (isTRUE(input$use_example)) {
      data("lalonde", package = "MatchIt", envir = environment())
      get("lalonde", envir = environment())
    } else {
      req(input$file)
      tryCatch(readr::read_csv(input$file$datapath, show_col_types = FALSE),
               error = function(e) { showNotification(e$message, type = "error"); NULL })
    }
  })

  data_clean <- reactive({
    df  <- data_raw(); req(df)
    grp <- input$grp_var
    if (!isTRUE(input$drop_na_ps) || is.null(grp) || !grp %in% names(df)) return(df)
    needed <- unique(c(grp, rv$sel))
    df[complete.cases(df[, needed, drop = FALSE]), ]
  })

  # grp를 0/1로 변환한 공유 데이터 (Results 탭 전체 사용)
  df_for_display <- reactive({
    df  <- data_clean(); req(df)
    grp <- input$grp_var; req(grp, grp %in% names(df))
    df[[grp]] <- as.integer(as.factor(df[[grp]])) - 1L
    df
  })

  # ── Data 탭 ───────────────────────────────────────────────────────────────────
  output$data_count <- renderText({
    df <- data_raw(); req(df); paste("총 데이터 개수:", nrow(df))
  })

  output$data_preview <- DT::renderDT({
    df <- data_raw(); req(df)
    DT::datatable(head(df, 30), options = list(scrollX = TRUE, pageLength = 10))
  })

  # ── 그룹변수 UI ───────────────────────────────────────────────────────────────
  output$ui_grp <- renderUI({
    df <- data_raw(); req(df)
    cands <- get_binary_candidates(df)
    if (length(cands) == 0) return(helpText("binary 변수가 없습니다."))
    selectInput("grp_var", "그룹변수 선택", choices = cands, selected = cands[1])
  })

  # ── 공변량 선택 상태 ──────────────────────────────────────────────────────────
  rv <- reactiveValues(sel = character(0))

  observeEvent(input$grp_var, { rv$sel <- character(0) })

  observeEvent(input$sel_changed, {
    v <- input$sel_changed$var
    if (isTRUE(input$sel_changed$checked)) rv$sel <- union(rv$sel, v)
    else                                    rv$sel <- setdiff(rv$sel, v)
  }, ignoreInit = TRUE)

  # ── Variable Selection 탭 ─────────────────────────────────────────────────────
  var_tbl_data <- reactive({
    df  <- data_raw(); grp <- input$grp_var
    req(df, grp, grp %in% names(df))
    compute_var_table(df, grp)
  })

  output$var_table <- DT::renderDT({
    dt         <- var_tbl_data(); req(nrow(dt) > 0)
    grp        <- input$grp_var;  req(grp)
    df         <- data_raw()
    grp_vals   <- df[[grp]]
    grp_levels <- as.character(sort(unique(na.omit(grp_vals))))
    sel_now    <- rv$sel

    # 그룹별 N 계산
    n_total <- nrow(df)
    n_grp   <- setNames(
      sapply(grp_levels, function(gl) sum(as.character(grp_vals) == gl, na.rm = TRUE)),
      grp_levels
    )

    dt$Select <- vapply(seq_len(nrow(dt)), function(i) {
      as.character(tags$input(
        type = "checkbox", class = "selChk",
        id   = paste0("sel_", dt$var[i]),
        checked = if (dt$var[i] %in% sel_now) "checked" else NULL
      ))
    }, character(1))

    dt$.__flag__ <- dplyr::case_when(
      is.na(dt$smd) ~ "ok", dt$smd > 0.1 ~ "imbal", TRUE ~ "ok"
    )

    # 컬럼명에 N= 포함
    overall_col <- paste0("Overall (N=", n_total, ")")
    out <- data.frame(
      Select   = dt$Select,
      Variable = dt$var,
      Type     = dt$type,
      N        = dt$N,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out[[overall_col]] <- dt$overall
    for (gl in grp_levels) {
      out[[paste0(gl, " (N=", n_grp[[gl]], ")")]] <- dt[[paste0("grp_", gl)]]
    }
    out[["p-value"]]   <- ifelse(is.na(dt$pval), "",
                                 ifelse(dt$pval < 0.001, "<0.001", sprintf("%.3f", dt$pval)))
    out[["|SMD|"]]     <- ifelse(is.na(dt$smd), NA_real_, round(dt$smd, 3))
    out[[".__flag__"]] <- dt$.__flag__

    flag_idx <- which(names(out) == ".__flag__") - 1

    DT::datatable(
      out,
      escape = FALSE, selection = "none", rownames = FALSE,
      options = list(
        scrollX = TRUE, pageLength = 25,
        columnDefs = list(list(visible = FALSE, targets = flag_idx))
      ),
      callback = DT::JS("
        table.on('change', 'input.selChk', function() {
          var id = $(this).attr('id');
          var v  = id.replace('sel_','');
          Shiny.setInputValue('sel_changed',
            {var: v, checked: $(this).is(':checked'), nonce: Math.random()},
            {priority: 'event'});
        });
      ")
    ) |> highlight_imbal_rows()
  })

  # ── PS 분석 실행 ──────────────────────────────────────────────────────────────
  ps_result <- eventReactive(input$run_ps, {
    df   <- data_clean(); grp  <- input$grp_var; covs <- rv$sel
    shiny::validate(
      shiny::need(length(covs) >= 1, "공변량을 1개 이상 선택하세요."),
      shiny::need(!is.null(grp) && grp %in% names(df), "그룹변수를 선택하세요.")
    )
    df[[grp]] <- as.integer(as.factor(df[[grp]])) - 1L
    f <- reformulate(covs, response = grp)
    if (input$ps_method == "matching") {
      caliper_val <- if (input$caliper == 0) NULL else input$caliper
      tryCatch(
        MatchIt::matchit(f, data = df, method = input$match_method,
                         distance = input$distance, ratio = as.integer(input$ratio),
                         caliper = caliper_val, estimand = "ATT"),
        error = function(e) { showNotification(paste("Matching 실패:", e$message), type = "error"); NULL }
      )
    } else {
      tryCatch(
        MatchIt::matchit(f, data = df, method = NULL,
                         distance = input$distance_iptw, estimand = input$estimand),
        error = function(e) { showNotification(paste("IPTW 실패:", e$message), type = "error"); NULL }
      )
    }
  })

  observeEvent(ps_result(), {
    req(ps_result())
    df  <- data_clean(); grp <- input$grp_var
    all_covs <- setdiff(names(df), grp)
    choices  <- c("PS Distribution" = ".__distance__", all_covs)
    updateSelectInput(session, "bal_var", choices = choices, selected = ".__distance__")
  })

  # ── Results 공통 ──────────────────────────────────────────────────────────────
  output$result_info <- renderUI({
    res <- ps_result(); req(res)
    method_label <- if (input$ps_method == "matching")
      paste0("Matching (", input$match_method, ", ratio=", input$ratio, ")")
    else paste0("IPTW (", input$estimand, ")")
    tags$p(tags$strong("분석 방법: "), method_label, tags$br(),
           tags$strong("공변량: "), paste(rv$sel, collapse = ", "))
  })

  # ── Love Plot ─────────────────────────────────────────────────────────────────
  output$love_plot <- renderPlot({
    res  <- ps_result(); req(res)
    df   <- df_for_display()
    grp  <- input$grp_var; covs <- rv$sel
    all_covs  <- setdiff(names(df), grp)
    addl_vars <- setdiff(all_covs, covs)
    addl_form <- if (length(addl_vars) > 0) reformulate(addl_vars) else NULL

    vline02 <- ggplot2::geom_vline(xintercept = 0.2, linetype = "dashed", color = "gray60")
    if (input$ps_method == "matching") {
      cobalt::love.plot(res, addl = addl_form, data = df,
                        threshold = 0.1, abs = TRUE, var.order = "unadjusted",
                        title = "Love Plot — Matching") + vline02
    } else {
      w <- compute_weights(res, input$estimand)
      cobalt::love.plot(res, addl = addl_form, data = df, weights = w,
                        threshold = 0.1, abs = TRUE, var.order = "unadjusted",
                        title = paste0("Love Plot — IPTW (", input$estimand, ")")) + vline02
    }
  })

  # ── Balance Plot ──────────────────────────────────────────────────────────────
  output$bal_plot <- renderPlot({
    res <- ps_result(); req(res, input$bal_var)
    df  <- df_for_display()

    if (input$bal_var == ".__distance__") {
      # PS(propensity score) 분포
      if (input$ps_method == "matching") {
        cobalt::bal.plot(res, var.name = "distance", which = "both")
      } else {
        w <- compute_weights(res, input$estimand)
        cobalt::bal.plot(res, var.name = "distance", which = "both", weights = w)
      }
    } else {
      # PS 공식 미포함 변수는 addl로 전달
      needs_addl  <- !(input$bal_var %in% rv$sel)
      addl_for_bal <- if (needs_addl) reformulate(input$bal_var) else NULL
      if (input$ps_method == "matching") {
        cobalt::bal.plot(res, var.name = input$bal_var, which = "both",
                         data = df, addl = addl_for_bal)
      } else {
        w <- compute_weights(res, input$estimand)
        cobalt::bal.plot(res, var.name = input$bal_var, which = "both",
                         weights = w, data = df, addl = addl_for_bal)
      }
    }
  })

  # ── Balance Table ─────────────────────────────────────────────────────────────
  output$bal_tbl <- DT::renderDT({
    res  <- ps_result(); req(res)
    df   <- df_for_display(); grp <- input$grp_var
    req(length(rv$sel) >= 1)
    all_covs   <- setdiff(names(df), grp)   # PS 포함/미포함 전체 공변량
    grp_levels <- c("0", "1")

    # ── Pre-match 통계 ──
    n_pre_total <- nrow(df)
    n_pre_grp <- setNames(
      sapply(grp_levels, function(gl) sum(df[[grp]] == as.integer(gl))),
      grp_levels
    )
    pre <- compute_balance_stats(df, grp, all_covs, weights = NULL)

    # ── Post-match 통계 ──
    if (input$ps_method == "matching") {
      matched_df <- tryCatch(MatchIt::match.data(res), error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(matched_df), "Matched data 생성 실패"))
      matched_df[[grp]] <- as.integer(as.factor(matched_df[[grp]])) - 1L
      n_post_total <- nrow(matched_df)
      n_post_grp <- setNames(
        sapply(grp_levels, function(gl) sum(matched_df[[grp]] == as.integer(gl))),
        grp_levels
      )
      post <- compute_balance_stats(matched_df, grp, all_covs, weights = matched_df$weights)
    } else {
      w       <- compute_weights(res, input$estimand)
      df_iptw <- res$model$data
      df_iptw[[grp]] <- as.integer(as.factor(df_iptw[[grp]])) - 1L
      n_post_total <- nrow(df_iptw)
      n_post_grp <- setNames(
        sapply(grp_levels, function(gl) sum(df_iptw[[grp]] == as.integer(gl))),
        grp_levels
      )
      post <- compute_balance_stats(df_iptw, grp, all_covs, weights = w)
    }

    # ── 출력 데이터프레임 조립 ──
    out <- data.frame(Variable = pre$var, Type = pre$type,
                      check.names = FALSE, stringsAsFactors = FALSE)

    # Pre-match 컬럼
    pre_overall_col  <- paste0("Overall (N=", n_pre_total, ")")
    out[[pre_overall_col]] <- pre$overall
    for (gl in grp_levels) {
      out[[paste0(gl, " (N=", n_pre_grp[[gl]], ")")]] <- pre[[paste0("grp_", gl)]]
    }
    out[["Pre |SMD|"]] <- ifelse(is.na(pre$smd), "", sprintf("%.3f", pre$smd))

    # Post-match 컬럼 (굵은 구분선은 첫 번째 post 컬럼에 CSS 클래스로)
    post_overall_col <- paste0("Overall (N=", n_post_total, ")")
    out[[post_overall_col]] <- post$overall
    for (gl in grp_levels) {
      out[[paste0(gl, " (N=", n_post_grp[[gl]], ")")]] <- post[[paste0("grp_", gl)]]
    }
    out[["Post |SMD|"]] <- ifelse(is.na(post$smd), "", sprintf("%.3f", post$smd))

    # SMD 강조 플래그 (Post |SMD| > 0.1 → 연노랑)
    out[[".__post_flag__"]] <- ifelse(is.na(post$smd) | post$smd <= 0.1, "ok", "imbal")

    n_pre_cols  <- 1 + length(grp_levels) + 1  # overall + groups + SMD
    post_start  <- 2 + n_pre_cols               # Variable, Type, [pre cols], [post cols start]
    flag_idx    <- ncol(out) - 1                # 0-indexed

    dt_widget <- DT::datatable(
      out,
      escape = FALSE, selection = "none", rownames = FALSE,
      options = list(
        scrollX = TRUE, pageLength = 30,
        columnDefs = list(
          list(visible = FALSE, targets = flag_idx),
          list(className = "thick-left bg-post", targets = post_start),
          list(className = "bg-post", targets = seq(post_start + 1, ncol(out) - 2))
        )
      )
    )

    # Pre 컬럼 배경색
    pre_col_names <- names(out)[seq(3, 2 + n_pre_cols)]
    dt_widget <- DT::formatStyle(dt_widget, pre_col_names, backgroundColor = "#f5f5f5")

    # Post |SMD| 강조
    DT::formatStyle(dt_widget, ".__post_flag__", target = "row",
                    backgroundColor = DT::styleEqual(c("ok","imbal"), c(NA, "lightyellow")),
                    fontWeight      = DT::styleEqual(c("ok","imbal"), c("normal","bold")))
  })
}

shinyApp(ui, server)
