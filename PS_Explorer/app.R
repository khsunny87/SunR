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
  library(survival)
  library(WeightIt)
  library(survey)
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

# Balance Table용: survey 패키지 기반 가중 mean±SD 또는 n(%)
fmt_stat_w <- function(x, w = NULL, is_cont) {
  if (is.null(w)) w <- rep(1, length(x))
  valid <- !is.na(x) & !is.na(w) & w > 0
  x <- x[valid]; w <- w[valid]
  if (length(x) == 0) return("")
  if (is_cont) {
    d   <- survey::svydesign(ids = ~1, data = data.frame(x = x), weights = ~w)
    wm  <- as.numeric(survey::svymean(~x, d))
    wsd <- sqrt(as.numeric(survey::svyvar(~x, d)))
    sprintf("%.1f ± %.1f", wm, wsd)
  } else {
    xf <- as.factor(x)
    d  <- survey::svydesign(ids = ~1, data = data.frame(x = xf), weights = ~w)
    props <- as.numeric(survey::svymean(~xf, d)) * 100
    ux <- levels(xf)
    tab_n <- table(x)
    paste(paste0(ux, ": ", tab_n[ux], " (", round(props, 1), "%)"), collapse = "; ")
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
      if (is.null(weights)) {
        abs(smd::smd(x = xc, g = factor(gc), na.rm = FALSE)$estimate)
      } else if (is_cont) {
        g1 <- gc == grp_levels[1]; g2 <- gc == grp_levels[2]
        calc_grp <- function(vals, ws) {
          d <- survey::svydesign(ids = ~1, data = data.frame(x = vals), weights = ~ws)
          list(m = as.numeric(survey::svymean(~x, d)),
               v = as.numeric(survey::svyvar(~x, d)))
        }
        s1 <- calc_grp(xc[g1], wc[g1]); s2 <- calc_grp(xc[g2], wc[g2])
        abs(s1$m - s2$m) / sqrt((s1$v + s2$v) / 2)
      } else {
        abs(smd::smd(x = xc, g = factor(gc), w = wc, na.rm = FALSE)$estimate)
      }
    }, error = function(e) NA_real_)

    row <- tibble::tibble(var = v, type = if (is_cont) "Cont." else "Cat.",
                          overall = overall_val, smd = smd_val)
    for (gl in grp_levels) row[[paste0("grp_", gl)]] <- grp_stats[[gl]]
    row
  })
}

# IPTW weight trimming (winsorize at pct, IPTW.R 참고)
apply_trim <- function(w, trim = TRUE, pct = 0.99) {
  if (!trim) return(w)
  pmin(w, quantile(w, pct, na.rm = TRUE))
}

# DT 행 강조 (우선순위: both > outcome > smd > ok)
highlight_imbal_rows <- function(dt_widget, flag_col = ".__flag__") {
  DT::formatStyle(
    dt_widget, flag_col, target = "row",
    backgroundColor = DT::styleEqual(
      c("ok", "smd",         "outcome",  "both"),
      c(NA,   "lightyellow", "#cce5ff",  "#c3e6cb")
    ),
    fontWeight = DT::styleEqual(
      c("ok",     "smd",  "outcome", "both"),
      c("normal", "bold", "bold",    "bold")
    )
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
    selectInput("example_ds", "예제 데이터 선택",
                choices = c("lalonde (MatchIt, N=614)" = "lalonde",
                            "colon (survival, N=1858)" = "colon"),
                selected = "lalonde")
  ),
  hr(),
  h4("2) 그룹변수"),
  uiOutput("ui_grp"),
  helpText("0/1 또는 2-레벨 변수만 표시됩니다."),
  hr(),
  h4("3) 아웃컴 회귀"),
  selectInput("outcome_type", "회귀 모델",
              choices = c("(선택 안 함)" = "", "Linear" = "linear",
                          "Logistic" = "logistic", "Cox" = "cox"),
              selected = ""),
  uiOutput("ui_outcome_var"),
  uiOutput("ui_time_var")
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
                           selected = "ATE", inline = TRUE, status = "info"),
        selectInput("weightit_method", "WeightIt Method",
                    choices = c("PS (Logistic)" = "ps",
                                "CBPS"          = "cbps",
                                "Entropy Bal."  = "ebal"),
                    selected = "ps"),
        prettySwitch("stabilize_weights", "Stabilized weights", value = FALSE, status = "info"),
        prettySwitch("trim_weights", "Weight trimming (99th pct)", value = FALSE, status = "warning")
      ),
      hr(),
      prettySwitch("drop_na_ps", "Complete-case (결측 행 제거)",
                   value = TRUE, status = "warning"),
      actionButton("run_ps", "Run Analysis", class = "btn-primary btn-lg", icon = icon("play")),
      br(), br(),
      verbatimTextOutput("ps_code_display")
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
      ds <- input$example_ds %||% "lalonde"
      if (ds == "colon") {
        data("colon", package = "survival", envir = environment())
        get("colon", envir = environment())
      } else {
        data("lalonde", package = "MatchIt", envir = environment())
        get("lalonde", envir = environment())
      }
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

  # ── 아웃컴 회귀 UI ────────────────────────────────────────────────────────────
  output$ui_outcome_var <- renderUI({
    req(nchar(input$outcome_type %||% "") > 0)
    df <- data_raw(); req(df)
    vars <- setdiff(names(df), input$grp_var %||% character(0))
    selectInput("outcome_var", "아웃컴 변수",
                choices = c("(선택)" = "", vars), selected = "")
  })

  output$ui_time_var <- renderUI({
    req(identical(input$outcome_type, "cox"))
    df <- data_raw(); req(df)
    exclude  <- c(input$grp_var %||% "", input$outcome_var %||% "")
    num_vars <- Filter(function(v) is.numeric(df[[v]]), setdiff(names(df), exclude))
    selectInput("time_var", "시간 변수 (Cox)",
                choices = c("(선택)" = "", num_vars), selected = "")
  })

  # ── 아웃컴 단변수 p-value ─────────────────────────────────────────────────────
  outcome_pvals <- reactive({
    ot <- input$outcome_type %||% ""
    ov <- input$outcome_var  %||% ""
    if (ot == "" || ov == "") return(NULL)
    df  <- data_raw(); if (is.null(df)) return(NULL)
    grp <- input$grp_var;     if (is.null(grp)) return(NULL)
    exclude <- c(grp, ov)
    if (ot == "cox") {
      tv <- input$time_var %||% ""
      if (tv == "") return(NULL)
      exclude <- c(exclude, tv)
    }
    covs <- setdiff(names(df), exclude)
    setNames(sapply(covs, function(v) {
      tryCatch({
        if (ot == "linear") {
          summary(lm(formula(paste(ov, "~", v)), data = df))$coefficients[2, 4]
        } else if (ot == "logistic") {
          summary(glm(formula(paste(ov, "~", v)), data = df,
                      family = binomial))$coefficients[2, 4]
        } else {
          summary(coxph(formula(paste0("Surv(", tv, ",", ov, ")~", v)),
                        data = df))$coefficients[1, 5]
        }
      }, error = function(e) NA_real_)
    }), covs)
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

    # outcome p-value 먼저 계산 (플래그에도 사용)
    op     <- outcome_pvals()
    pv_vec <- if (is.null(op)) rep(NA_real_, nrow(dt)) else as.numeric(op[dt$var])

    # 4단계 플래그: both > outcome > smd > ok
    smd_high <- !is.na(dt$smd) & dt$smd > 0.1
    out_sig  <- !is.na(pv_vec) & pv_vec < 0.05
    flag_vals <- dplyr::case_when(
      smd_high & out_sig ~ "both",
      out_sig            ~ "outcome",
      smd_high           ~ "smd",
      TRUE               ~ "ok"
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
    out[["Outcome p"]] <- ifelse(is.na(pv_vec), "",
                                 ifelse(pv_vec < 0.001, "<0.001", sprintf("%.3f", pv_vec)))
    out[[".__flag__"]] <- flag_vals

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
        WeightIt::weightit(f, data = df, method = input$weightit_method,
                           estimand = input$estimand,
                           stabilize = isTRUE(input$stabilize_weights)),
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

  # ── Method 탭 코드 박스 ───────────────────────────────────────────────────────
  output$ps_code_display <- renderText({
    res <- ps_result(); req(res)
    grp  <- isolate(input$grp_var)
    covs <- isolate(rv$sel)
    f_str <- paste0(grp, " ~ ", paste(covs, collapse = " + "))
    if (isolate(input$ps_method) == "matching") {
      caliper_val <- isolate(input$caliper)
      cal_str <- if (caliper_val == 0) "" else paste0(",\n        caliper = ", caliper_val)
      paste0(
        "matchit(", f_str, ",\n",
        "        data = df,\n",
        "        method = \"", isolate(input$match_method), "\",\n",
        "        distance = \"", isolate(input$distance), "\",\n",
        "        ratio = ", isolate(input$ratio), cal_str, ",\n",
        "        estimand = \"ATT\")"
      )
    } else {
      stab_str <- if (isTRUE(isolate(input$stabilize_weights)))
        ",\n         stabilize = TRUE" else ""
      trim_str <- if (isTRUE(isolate(input$trim_weights))) paste0(
        "\n# Weight trimming (99th pct winsorization)\n",
        "w <- res$weights\n",
        "w <- pmin(w, quantile(w, 0.99))"
      ) else ""
      paste0(
        "res <- weightit(", f_str, ",\n",
        "         data = df,\n",
        "         method = \"", isolate(input$weightit_method), "\",\n",
        "         estimand = \"", isolate(input$estimand), "\"",
        stab_str, ")",
        trim_str
      )
    }
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
      res_show <- res
      res_show$weights <- apply_trim(res$weights, isTRUE(input$trim_weights))
      cobalt::love.plot(res_show, addl = addl_form, data = df,
                        threshold = 0.1, abs = TRUE, var.order = "unadjusted",
                        title = paste0("Love Plot — IPTW (", input$estimand, ")")) + vline02
    }
  })

  # ── Balance Plot ──────────────────────────────────────────────────────────────
  output$bal_plot <- renderPlot({
    res <- ps_result(); req(res, input$bal_var)
    df  <- df_for_display()

    if (input$bal_var == ".__distance__") {
      # PS 분포
      if (input$ps_method == "matching") {
        cobalt::bal.plot(res, var.name = "distance", which = "both")
      } else {
        w_trim <- apply_trim(res$weights, isTRUE(input$trim_weights))
        ps_df <- data.frame(ps = res$ps, group = factor(df[[input$grp_var]]), w = w_trim)
        df_pre  <- transform(ps_df, weight = 1,   panel = "Unadjusted Sample")
        df_post <- transform(ps_df, weight = w,   panel = "Adjusted Sample (IPTW)")
        plot_df <- rbind(df_pre, df_post)
        plot_df$panel <- factor(plot_df$panel,
                                levels = c("Unadjusted Sample", "Adjusted Sample (IPTW)"))
        ggplot2::ggplot(plot_df, ggplot2::aes(x = ps, weight = weight, fill = group)) +
          ggplot2::geom_density(alpha = 0.4) +
          ggplot2::facet_wrap(~panel) +
          ggplot2::labs(x = "Propensity Score", y = "Density",
                        title = 'Distributional Balance for "prop.score"') +
          ggplot2::theme_bw()
      }
    } else {
      needs_addl   <- !(input$bal_var %in% rv$sel)
      addl_for_bal <- if (needs_addl) reformulate(input$bal_var) else NULL
      if (input$ps_method == "matching") {
        cobalt::bal.plot(res, var.name = input$bal_var, which = "both",
                         data = df, addl = addl_for_bal)
      } else {
        res_show <- res
        res_show$weights <- apply_trim(res$weights, isTRUE(input$trim_weights))
        cobalt::bal.plot(res_show, var.name = input$bal_var, which = "both",
                         data = df, addl = addl_for_bal)
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
      w       <- apply_trim(res$weights, isTRUE(input$trim_weights))
      df_iptw <- df
      # Σw — svyCreateTableOne의 n과 동일
      wsum_total <- round(sum(w), 1)
      wsum_grp   <- setNames(sapply(grp_levels, function(gl) {
        round(sum(w[df_iptw[[grp]] == as.integer(gl)]), 1)
      }), grp_levels)
      post <- compute_balance_stats(df_iptw, grp, all_covs, weights = w)
    }

    # ── 출력 데이터프레임 조립 ──
    out <- data.frame(
      Variable = ifelse(pre$var %in% rv$sel, paste0("✔ ", pre$var), pre$var),
      Type = pre$type,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    is_iptw <- input$ps_method != "matching"

    # Pre 컬럼 (Matching: "Pre |SMD|", IPTW: "Unadj. |SMD|")
    pre_overall_col  <- paste0("Overall (N=", n_pre_total, ")")
    out[[pre_overall_col]] <- pre$overall
    for (gl in grp_levels) {
      out[[paste0(gl, " (N=", n_pre_grp[[gl]], ")")]] <- pre[[paste0("grp_", gl)]]
    }
    pre_smd_col <- if (is_iptw) "Unadj. |SMD|" else "Pre |SMD|"
    out[[pre_smd_col]] <- ifelse(is.na(pre$smd), "", sprintf("%.3f", pre$smd))

    # Post 컬럼 — Matching: N=, IPTW: ESS=
    post_overall_col <- if (is_iptw)
      paste0("Overall (Σw=", wsum_total, ")")
    else
      paste0("Overall (N=", n_post_total, ")")
    out[[post_overall_col]] <- post$overall
    for (gl in grp_levels) {
      post_grp_label <- if (is_iptw)
        paste0(gl, " (Σw=", wsum_grp[[gl]], ")")
      else
        paste0(gl, " (N=", n_post_grp[[gl]], ")")
      out[[post_grp_label]] <- post[[paste0("grp_", gl)]]
    }
    post_smd_col <- if (is_iptw) "IPTW |SMD|" else "Post |SMD|"
    out[[post_smd_col]] <- ifelse(is.na(post$smd), "", sprintf("%.3f", post$smd))

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
