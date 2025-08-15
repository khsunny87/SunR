# app.R
# ---------------------------------------------
# Interactive Regression Workbench (Modular)
# - Front-end UI is identical to your prototype
# - Back-end is modular via settings.json + model_registry.R + models/*.R
# ---------------------------------------------
base::suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(broom)
  library(MASS)
  library(survival)
})
# ----- Registry load -----
source("model_registry.R")
.settings <- load_model_settings("settings.json")
register_models(.settings)

# Build choices for model_type (keep the same labels/ids order as settings.json)
.model_choices <- setNames(vapply(.settings, `[[`, "", "id"),
                           vapply(.settings, `[[`, "", "label"))
# Ensure the same visible labels as before: Linear / Logistic / Cox
# (The order/labels should match settings.json; otherwise override here.)

# ---------- UI ----------
sidebar <- sidebarPanel(
  width = 4,
  h4("1) Data"),
  fileInput("file", "Upload CSV (utf-8)", accept = c(".csv", "text/csv", "text/plain")),
  prettySwitch("use_example", "Use example data", value = TRUE, status = "info"),
  conditionalPanel(
    condition = "input.use_example == true",
    helpText("Examples: mtcars (for linear/logistic), lung (for Cox)")
  ),
  hr(),
  
  h4("2) Outcome & Model"),
  selectInput(
    "model_type", "Model type",
    choices = .model_choices,  # labels from settings.json
    selected = if ("linear" %in% .model_choices) "linear" else names(.model_choices)[1]
  ),
  
  uiOutput("outcome_ui"),
  uiOutput("surv_ui"),
  hr(),
  
  h4("3) Candidate Covariates"),
  uiOutput("covariate_list"),
  hr(),
  
  actionButton("save_snapshot", "Save Snapshot"),
  textInput("snapshot_label", NULL, placeholder = "Optional label for snapshot"),
  downloadButton("download_model", "Download model (.rds)"),
  downloadButton("download_table", "Download table (.csv)")
)

main <- mainPanel(
  width = 8,
  tabsetPanel(
    tabPanel("Data",
             h4("Preview"),
             textOutput("data_count"),
             tags$hr(),
             DTOutput("data_preview")),
    tabPanel("Variable selection",
             helpText("Each candidate covariate is fit in a univariable model with the chosen outcome."),
             DTOutput("univ_table")),
    tabPanel("Diagnosis",
             helpText("Multicollinearity check among currently selected covariates (rv$sel + rv$force)."),
             DTOutput("vif_table")),
    tabPanel(
      "Model result",
      h4("Model formula"),
      verbatimTextOutput("formula_txt"),
      h4("Cleaning"),
      checkboxInput(
        "drop_na",
        HTML("<span style='white-space: nowrap;'>Complete-case analysis (drop rows with missing in selected vars)</span>"),
        TRUE
      ),
      tableOutput("na_info"),
      hr(),
      
      h4("Stepwise (optional)"),
      prettyRadioButtons(
        "step_direction", "Direction",
        choices = c("none", "forward", "backward", "both"),
        selected = "none", inline = TRUE
      ),
      prettyRadioButtons(
        "criterion", "Criterion",
        choices = c("AIC", "BIC"),
        selected = "AIC", inline = TRUE
      ),
      actionButton("fit", "Fit Model", class = "btn-primary"),
      hr(),
      
      h5("Coefficients"),
      DTOutput("coef_table"),
      h5("Model diagnostics / notes"),
      verbatimTextOutput("model_notes")
    ),
    tabPanel("Saved results",
             DTOutput("snapshots"),
             downloadButton("download_snapshots", "Download all snapshots (.csv)"))
  )
)

ui <- fluidPage(
  titlePanel("Interactive Regression Workbench"),
  sidebarLayout(sidebar, main)
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # --- Helpers / Styles ---
  highlight_sig_rows <- function(dt_widget, sig_col = ".__sig__") {
    DT::formatStyle(
      dt_widget,
      sig_col, target = "row",
      backgroundColor = DT::styleEqual(c("ns", "sig"), c(NA, "yellow")),
      color          = DT::styleEqual(c("ns", "sig"), c(NA, "red")),
      fontWeight     = DT::styleEqual(c("ns", "sig"), c("normal", "bold"))
    )
  }
  
  rv <- reactiveValues(sel = character(0), force = character(0))
  fit_store <- reactiveVal(NULL)
  
  # --- Example or Uploaded Data ---
  data_ex <- reactive({
    if (isTRUE(input$use_example)) {
      mod <- get_model(input$model_type)
      if (isTRUE(mod$time_based)) {
        lung %>% dplyr::mutate(status = ifelse(status == 2, 1, 0))
      } else {
        mtcars %>% dplyr::mutate(across(where(is.numeric), as.numeric))
      }
    } else {
      req(input$file)
      readr::read_csv(input$file$datapath, show_col_types = FALSE)
    }
  })
  
  # --- Outcome / Survival UI from model meta ---
  output$outcome_ui <- renderUI({
    mod <- get_model(input$model_type)
    if (isTRUE(mod$time_based)) return(NULL)  # 시간기반 모델이면 outcome 직접 선택 UI 숨김
    df <- data_ex()
    selectInput("outcome", "Outcome variable", choices = names(df))
  })
  
  
  output$surv_ui <- renderUI({
    mod <- get_model(input$model_type)
    if (!isTRUE(mod$time_based)) return(NULL)  # 시간기반 모델일 때만 노출
    df <- data_ex(); cols <- names(df)
    tagList(
      selectInput("time_col",  "Survival time column", choices = cols),
      selectInput("event_col", "Event indicator column (0/1)", choices = cols),
      checkboxInput("nonzero_event", "Treat non-zero as event (coerce to 0/1)", TRUE)
    )
  })
  
  
  # --- Safe wrappers ---
  safe_glm <- function(form, data, family = NULL) {
    tryCatch({
      if (is.null(family)) glm(form, data = data) else glm(form, data = data, family = family)
    }, error = function(e) NULL)
  }
  safe_cox <- function(form, data) {
    tryCatch({ coxph(form, data = data) }, error = function(e) NULL)
  }
  
  # --- Data cleaning ---
  cleaned_data <- reactive({
    df <- data_ex()
    needed <- character(0)
    mod <- get_model(input$model_type)
    
    if (isTRUE(mod$time_based)) {
      req(input$time_col, input$event_col)
      needed <- c(input$time_col, input$event_col, rv$sel, rv$force)
    } else {
      req(input$outcome)
      needed <- c(input$outcome, rv$sel, rv$force)
    }
    needed <- unique(needed[needed != ""])
    
    df2 <- df
    if (identical(input$model_type, "cox") && !is.null(input$event_col)) {
      if (isTRUE(input$nonzero_event)) {
        df2[[input$event_col]] <- as.integer(df2[[input$event_col]] != 0)
      }
    }
    if (isTRUE(input$drop_na) && length(needed) > 0) {
      df2 <- df2[complete.cases(df2[, needed, drop = FALSE]), ]
    }
    df2
  })
  
  # --- Cleaning stats ---
  output$na_info <- renderTable({
    df  <- data_ex()
    df2 <- cleaned_data()
    data.frame(
      Metric = c("Original N", "Cleaned N", "Dropped"),
      Value  = c(nrow(df), nrow(df2), nrow(df) - nrow(df2)),
      check.names = FALSE
    )
  }, rownames = FALSE, align = "lr")
  
  output$data_count <- renderText({
    req(data_ex()); paste("총 데이터 개수:", nrow(data_ex()))
  })
  
  output$data_preview <- renderDT({
    datatable(head(data_ex(), 30), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # --- Selected covariates list ---
  output$covariate_list <- renderUI({
    sel <- rv$sel; force <- rv$force
    if (length(sel) == 0) {
      return(tags$p("No covariates selected yet. Use the checkboxes in the 'Variable selection' table."))
    }
    items <- vapply(sel, function(v) if (v %in% force) paste0(v, " *") else v, character(1))
    tagList(tags$ul(lapply(items, function(x) tags$li(x))), tags$p(tags$small("* force-in")))
  })
  
  # --- VIF (term-level, shared) ---
  vif_shared <- reactive({
    df <- cleaned_data()
    vars <- unique(c(rv$force, rv$sel))
    if (length(vars) < 2)
      return(tibble::tibble(Variable = character(0), VIF = double(0), R2 = double(0), Note = character(0)))
    
    form <- as.formula(paste0("~ ", paste(vars, collapse = " + ")))
    X <- tryCatch(model.matrix(form, data = df), error = function(e) NULL)
    if (is.null(X))
      return(tibble::tibble(Variable = character(0), VIF = double(0), R2 = double(0), Note = character(0)))
    
    assign_vec <- attr(X, "assign")
    if (!is.null(assign_vec) && any(assign_vec == 0)) {
      X <- X[, assign_vec != 0, drop = FALSE]
      assign_vec <- assign_vec[assign_vec != 0]
    }
    keep <- apply(X, 2, function(col) stats::var(col, na.rm = TRUE) > 0)
    X <- X[, keep, drop = FALSE]; assign_vec <- assign_vec[keep]
    if (ncol(X) < 2)
      return(tibble::tibble(Variable = character(0), VIF = double(0), R2 = double(0), Note = character(0)))
    
    term_labels <- attr(terms(form), "term.labels")
    present_terms <- sort(unique(assign_vec))
    if (length(present_terms) < 2)
      return(tibble::tibble(Variable = character(0), VIF = double(0), R2 = double(0), Note = character(0)))
    
    rows <- lapply(present_terms, function(k) {
      cols_k <- which(assign_vec == k)
      cols_other <- which(assign_vec != k)
      vifs_k <- sapply(cols_k, function(j) {
        y <- X[, j]; Z <- X[, cols_other, drop = FALSE]
        r2 <- tryCatch(summary(stats::lm(y ~ Z))$r.squared, error = function(e) NA_real_)
        if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
      })
      vif_term <- suppressWarnings(max(vifs_k, na.rm = TRUE))
      if (!is.finite(vif_term)) vif_term <- Inf
      r2_term <- if (is.finite(vif_term)) max(0, 1 - 1 / vif_term) else 1
      data.frame(
        Variable = term_labels[k],
        VIF = as.numeric(vif_term),
        R2  = as.numeric(r2_term),
        Note = "",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(rows) %>% dplyr::arrange(dplyr::desc(VIF))
  })
  
  # ---- REPLACE: VIF table renderer ----
  output$vif_table <- DT::renderDT({
    df <- vif_shared()
    
    # 방어: 데이터 프레임이 아니거나 행이 없으면 안내만 보여줌
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
      msg <- data.frame(
        Message = "Select at least two covariates to compute VIF (rv$sel + rv$force).",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      return(DT::datatable(
        msg,
        options = list(dom = 't', paging = FALSE),
        rownames = FALSE
      ))
    }
    
    dt <- DT::datatable(
      df,
      options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE
    )
    
    DT::formatStyle(
      dt, "VIF",
      backgroundColor = DT::styleInterval(c(5, 10), c(NA, "khaki", "salmon")),
      fontWeight      = DT::styleInterval(c(5, 10), c("normal", "bold", "bold"))
    )
  })
  # ---- END REPLACE ----
  
  
  
  
  
  # --- Univariable screening ---
  univ_tbl <- reactive({
    df  <- cleaned_data()
    mod <- get_model(input$model_type)
    
    # 제외할 컬럼: time_based면 time/event, 아니면 outcome
    drop_cols <- if (isTRUE(mod$time_based)) c(input$time_col, input$event_col) else input$outcome
    cand <- setdiff(names(df), drop_cols)
    
    # 안전 래퍼
    safe_glm <- function(form, data, family = NULL) {
      tryCatch({
        if (is.null(family)) stats::glm(form, data = data) else stats::glm(form, data = data, family = family)
      }, error = function(e) NULL)
    }
    safe_cox <- function(form, data) {
      tryCatch({ survival::coxph(form, data = data) }, error = function(e) NULL)
    }
    
    res <- lapply(cand, function(v) {
      # 공식
      f <- if (isTRUE(mod$time_based)) {
        stats::as.formula(paste0("Surv(", input$time_col, ", ", input$event_col, ") ~ ", v))
      } else {
        stats::as.formula(paste0(input$outcome, " ~ ", v))
      }
      
      # 피팅 (time_based / logistic / linear)
      fit <- if (isTRUE(mod$time_based)) {
        safe_cox(f, df)
      } else if (identical(input$model_type, "logistic")) {
        safe_glm(f, df, family = stats::binomial())
      } else {
        safe_glm(f, df)
      }
      
      # 사용된 관측치 수
      n_obs <- tryCatch({
        if (inherits(fit, "coxph")) as.integer(fit$n)
        else if (!is.null(fit) && !is.null(fit$model)) nrow(fit$model)
        else if (!is.null(fit) && !is.null(fit$y)) NROW(fit$y)
        else NA_integer_
      }, error = function(e) NA_integer_)
      
      # 실패 처리
      if (is.null(fit)) {
        return(tibble::tibble(var = v, N = n_obs, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
      }
      
      tdy <- tryCatch(broom::tidy(fit), error = function(e) NULL)
      if (is.null(tdy) || !"estimate" %in% names(tdy)) {
        return(tibble::tibble(var = v, N = n_obs, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
      }
      
      # 절편 제거 (있으면)
      if ("term" %in% names(tdy)) {
        tdy <- tdy[tdy$term != "(Intercept)", , drop = FALSE]
      }
      if (nrow(tdy) == 0) {
        return(tibble::tibble(var = v, N = n_obs, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
      }
      
      est <- tdy$estimate[1]
      se  <- tdy$std.error[1]
      p   <- tdy$p.value[1]
      
      # 라벨 & 효과 변환
      note <- if (isTRUE(mod$time_based)) "HR" else if (identical(input$model_type, "logistic")) "OR" else "Beta"
      eff  <- if (note %in% c("OR","HR")) exp(est) else est
      
      tibble::tibble(var = v, N = n_obs, effect = eff, se = se, p = p, note = note)
    })
    
    final <- purrr::compact(res) %>% dplyr::bind_rows()
    if (!("N" %in% names(final))) final$N <- NA_integer_
    final
  })
  
  
  output$univ_table <- renderDT({
    dt <- univ_tbl()
    req(!is.null(dt), nrow(dt) > 0)
    
    vif_df <- vif_shared() %>% dplyr::select(Variable, VIF)
    dt <- dt %>% dplyr::left_join(vif_df, by = c("var" = "Variable"))
    if (!("N"   %in% names(dt)))  dt$N   <- NA_integer_
    if (!("VIF" %in% names(dt)))  dt$VIF <- NA_real_
    
    sel_now   <- rv$sel
    force_now <- rv$force
    
    base <- dt %>% dplyr::select(var, N, effect, se, p, note, VIF)
    
    base$Select <- vapply(
      base$var,
      function(v) as.character(shiny::tags$input(
        type = "checkbox", class = "selChk",
        id = paste0("sel_", v),
        checked = if (v %in% sel_now) "checked" else NULL
      )),
      character(1)
    )
    
    base$`Force-in` <- vapply(
      base$var,
      function(v) {
        disabled <- !(v %in% sel_now)
        as.character(shiny::tags$input(
          type = "checkbox", class = "forceChk",
          id = paste0("force_", v),
          checked  = if (v %in% force_now) "checked" else NULL,
          disabled = if (disabled) "disabled" else NULL
        ))
      },
      character(1)
    )
    
    out <- base %>%
      dplyr::mutate(
        CI_low  = ifelse(is.na(effect), NA, effect - 1.96 * se),
        CI_high = ifelse(is.na(effect), NA, effect + 1.96 * se),
        `p-value` = dplyr::case_when(
          is.na(p) ~ NA_character_,
          p < 0.001 ~ "<0.001",
          TRUE ~ sprintf("%.3f", p)
        ),
        .__sig__ = dplyr::case_when(!is.na(p) & p < 0.05 ~ "sig", TRUE ~ "ns")
      ) %>%
      dplyr::transmute(
        Select, `Force-in`,
        Variable = var,
        N        = N,
        Effect   = round(effect, 4),
        `95% CI` = ifelse(is.na(CI_low), NA_character_,
                          paste0("(", round(CI_low, 4), ", ", round(CI_high, 4), ")")),
        `p-value`,
        VIF      = ifelse(is.na(VIF), NA, round(VIF, 3)),
        .__sig__
      )
    
    eff_name_by_type <- list(linear = "\u03B2", logistic = "OR", cox = "HR")
    names(out)[names(out) == "Effect"] <- eff_name_by_type[[input$model_type]] %||% "Effect"
    
    sig_idx <- which(names(out) == ".__sig__") - 1
    
    DT::datatable(
      out,
      escape = FALSE, selection = "none", rownames = FALSE,
      options = list(
        scrollX = TRUE, pageLength = 10,
        ordering = TRUE, order = list(),
        columnDefs = list(list(visible = FALSE, targets = sig_idx))
      ),
      callback = DT::JS("
        table.on('change', 'input.selChk', function() {
          var id = $(this).attr('id');
          var v  = id.replace('sel_','');
          var checked = $(this).is(':checked');
          Shiny.setInputValue('sel_changed', {var: v, checked: checked, nonce: Math.random()}, {priority: 'event'});
        });
        table.on('change', 'input.forceChk', function() {
          var id = $(this).attr('id');
          var v  = id.replace('force_','');
          var checked = $(this).is(':checked');
          Shiny.setInputValue('force_changed', {var: v, checked: checked, nonce: Math.random()}, {priority: 'event'});
        });
      ")
    ) %>% highlight_sig_rows()
  })
  
  observeEvent(input$sel_changed, {
    v <- input$sel_changed$var
    if (isTRUE(input$sel_changed$checked)) {
      rv$sel <- union(rv$sel, v)
    } else {
      rv$sel   <- setdiff(rv$sel, v)
      rv$force <- setdiff(rv$force, v)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$force_changed, {
    v <- input$force_changed$var
    if (!(v %in% rv$sel)) return()
    if (isTRUE(input$force_changed$checked)) {
      rv$force <- union(rv$force, v)
    } else {
      rv$force <- setdiff(rv$force, v)
    }
  }, ignoreInit = TRUE)
  
  # --- Formula via module ---
  current_formula <- reactive({
    mod <- get_model(input$model_type)
    mod$build_formula(input, rv)
  })
  
  output$formula_txt <- renderPrint({ current_formula() })
  
  # --- Fit + Stepwise (modular) ---
  observeEvent(input$fit, {
    df   <- cleaned_data()
    form <- current_formula()
    mod  <- get_model(input$model_type)
    
    # 예측자 점검(Cox는 최소 1개 필요)
    no_predictors <- identical(attr(terms(form), "term.labels"), character(0))
    if (isTRUE(mod$time_based) && no_predictors) {
      showNotification("Cox PH requires at least one predictor. Select covariates first.", type = "warning")
      fit_store(NULL)
      return(invisible(NULL))
    }
    
    # === base fit (tryCatch로 감싸고 validate 제거) ===
    base_fit <- tryCatch(
      mod$fit(form, df, input),
      error = function(e) {
        showNotification(paste0("Model failed to fit: ", e$message), type = "error")
        NULL
      }
    )
    if (is.null(base_fit)) {
      fit_store(NULL)
      return(invisible(NULL))   # << validate(need(...)) 대신 명시적으로 종료
    }
    
    # (이하 그대로 유지)
    df_df <- as.data.frame(df)
    assign(".step_df", df_df, envir = .GlobalEnv)
    on.exit({
      if (exists(".step_df", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".step_df", envir = .GlobalEnv)
      }
    }, add = TRUE)
    base_fit$call$data <- as.name(".step_df")
    f <- formula(base_fit); environment(f) <- .GlobalEnv
    base_fit$call$formula <- f
    if (!is.null(base_fit$terms)) attr(base_fit$terms, ".Environment") <- .GlobalEnv
    
    force_vars <- rv$force
    all_vars   <- unique(c(rv$force, rv$sel))
    k_val      <- if (input$criterion == "AIC") 2 else log(nrow(df_df))
    
    if (isTRUE(mod$time_based)) {
      lhs <- paste0("Surv(", input$time_col, ", ", input$event_col, ")")
      lower_form <- if (length(force_vars) > 0) as.formula(paste(lhs, "~", paste(force_vars, collapse = " + "))) else as.formula(paste(lhs, "~ 1"))
      upper_form <- if (length(all_vars)  > 0) as.formula(paste(lhs, "~", paste(all_vars,  collapse = " + "))) else as.formula(paste(lhs, "~ 1"))
    } else {
      lhs <- input$outcome
      lower_form <- if (length(force_vars) > 0) reformulate(force_vars, response = lhs) else as.formula(paste(lhs, "~ 1"))
      upper_form <- if (length(all_vars)  > 0) reformulate(all_vars,  response = lhs) else as.formula(paste(lhs, "~ 1"))
    }
    environment(lower_form) <- .GlobalEnv
    environment(upper_form) <- .GlobalEnv
    
    if (input$step_direction == "none" || no_predictors || !isTRUE(mod$supports_step)) {
      fit_store(base_fit)
      return(invisible(NULL))
    }
    
    step_fit <- tryCatch({
      run_stepwise(mod, base_fit, lower_form, upper_form, k_val, input$step_direction)
    }, error = function(e) {
      showNotification(paste("Stepwise selection 실패:", e$message), type = "error")
      NULL
    })
    
    fit_store(step_fit %||% base_fit)
  })
  
  
  
  # --- Final VIF for fitted model ---
  vif_final <- reactive({
    fit <- fit_store()
    df  <- cleaned_data()
    if (is.null(fit)) return(tibble::tibble(Variable = character(0), VIF = double(0)))
    X <- tryCatch(model.matrix(formula(fit), data = df), error = function(e) NULL)
    if (is.null(X)) return(tibble::tibble(Variable = character(0), VIF = double(0)))
    if ("(Intercept)" %in% colnames(X)) X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    keep <- apply(X, 2, function(col) stats::var(col, na.rm = TRUE) > 0)
    X <- X[, keep, drop = FALSE]
    if (ncol(X) < 1) return(tibble::tibble(Variable = character(0), VIF = double(0)))
    
    vifs <- sapply(seq_len(ncol(X)), function(j) {
      y <- X[, j]; Z <- X[, -j, drop = FALSE]
      r2 <- tryCatch(summary(stats::lm(y ~ Z))$r.squared, error = function(e) NA_real_)
      if (is.na(r2) || r2 >= 1) Inf else 1/(1 - r2)
    })
    tibble::tibble(Variable = colnames(X), VIF = as.numeric(vifs))
  })
  
  # --- Coef table ---

  # ---- REPLACE THIS WHOLE coef_table() BLOCK ----
  coef_table <- reactive({
    fit <- fit_store()
    if (is.null(fit)) return(tibble::tibble()) # 모델 없으면 렌더 중단
    
    # 관측치 N
    n_obs <- tryCatch({
      if (inherits(fit, "coxph")) as.integer(fit$n)
      else if (!is.null(fit))     as.integer(stats::nobs(fit))
      else NA_integer_
    }, error = function(e) NA_integer_)
    
    # tidy 안전 호출
    tdy <- tryCatch({
      broom::tidy(fit, conf.int = FALSE)
    }, error = function(e) NULL)
    
    # tidy 실패/NULL → 빈 테이블
    if (is.null(tdy)) {
      return(tibble::tibble(term=character(), metric=character(), effect=double(),
                            CI_low=double(), CI_high=double(), p_num=double(),
                            p=character(), VIF=double(), N=integer()))
    }
    
    # term 컬럼 없으면 빈 테이블 반환
    if (!("term" %in% names(tdy))) {
      return(tibble::tibble(term=character(), metric=character(), effect=double(),
                            CI_low=double(), CI_high=double(), p_num=double(),
                            p=character(), VIF=double(), N=integer()))
    }
    
    # 절편 제거 (base R 서브셋; dplyr NSE 회피)
    tdy <- tdy[tdy$term != "(Intercept)", , drop = FALSE]
    
    # 절편만 있거나 0행이면 빈 테이블 반환
    if (nrow(tdy) == 0) {
      return(tibble::tibble(term=character(), metric=character(), effect=double(),
                            CI_low=double(), CI_high=double(), p_num=double(),
                            p=character(), VIF=double(), N=integer()))
    }
    
    # 안전: 누락 컬럼 NA 채움
    if (!("estimate"  %in% names(tdy))) tdy$estimate  <- NA_real_
    if (!("std.error" %in% names(tdy))) tdy$std.error <- NA_real_
    if (!("p.value"   %in% names(tdy))) tdy$p.value   <- NA_real_
    
    # 효과/CI 계산
    if (inherits(fit, "coxph")) {
      metric <- "HR"
      effect <- exp(tdy$estimate)
      CI_low <- exp(tdy$estimate - 1.96 * tdy$std.error)
      CI_high<- exp(tdy$estimate + 1.96 * tdy$std.error)
    } else if (!is.null(stats::family(fit)) && stats::family(fit)$family == "binomial") {
      metric <- "OR"
      effect <- exp(tdy$estimate)
      CI_low <- exp(tdy$estimate - 1.96 * tdy$std.error)
      CI_high<- exp(tdy$estimate + 1.96 * tdy$std.error)
    } else {
      metric <- "Beta"
      effect <- tdy$estimate
      CI_low <- tdy$estimate - 1.96 * tdy$std.error
      CI_high<- tdy$estimate + 1.96 * tdy$std.error
    }
    
    # 표시용 p
    pnum <- tdy$p.value
    pfmt <- ifelse(is.na(pnum), NA_character_,
                   ifelse(pnum < 0.001, "<0.001", sprintf("%.3f", pnum)))
    
    # VIF 매칭
    vdf <- vif_final()
    out_vif <- dplyr::left_join(
      tibble::tibble(term = tdy$term, metric = metric, effect = effect,
                     CI_low = CI_low, CI_high = CI_high,
                     p_num = pnum, p = pfmt),
      vdf, by = c("term" = "Variable")
    )
    
    # 남은 NA VIF는 설계행렬 컬럼명으로 재시도
    if (any(is.na(out_vif$VIF))) {
      X <- tryCatch(stats::model.matrix(stats::formula(fit), data = cleaned_data()), error = function(e) NULL)
      if (!is.null(X)) {
        cn <- colnames(X)
        guess <- vapply(out_vif$term, function(tt) {
          hit <- which(cn == tt); if (length(hit) == 1) cn[hit] else NA_character_
        }, character(1))
        out_vif$term_col <- ifelse(is.na(guess), out_vif$term, guess)
        out_vif <- dplyr::left_join(out_vif, vdf, by = c("term_col" = "Variable"), suffix = c("", ".v2"))
        out_vif$VIF <- ifelse(is.na(out_vif$VIF), out_vif$VIF.v2, out_vif$VIF)
        out_vif$VIF.v2 <- NULL
      }
    }
    
    out_vif$N <- n_obs
    out_vif
  })
  # ---- END REPLACEMENT ----
  
  # ---- OPTIONAL: renderDT도 빈 데이터 대응을 좀 더 명확히 ----
  output$coef_table <- renderDT({
    df <- coef_table()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No coefficients to display (model has no predictors or failed)."),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    eff_name <- if ("OR" %in% df$metric) "OR" else if ("HR" %in% df$metric) "HR" else "\u03B2"
    pnum <- df$p_num
    pfmt <- ifelse(!is.na(pnum) & pnum < 0.001, "<0.001",
                   ifelse(!is.na(pnum), sprintf("%.3f", pnum), as.character(df$p)))
    sig_flag <- ifelse(!is.na(pnum) & pnum < 0.05, "sig", "ns")
    
    n_rows <- nrow(df)
    N_col   <- if ("N"   %in% names(df)) df$N else rep(NA_integer_, n_rows)
    VIF_col <- if ("VIF" %in% names(df)) round(df$VIF, 3) else rep(NA_real_,   n_rows)
    
    out <- data.frame(
      Variable = df$term,
      N        = N_col,
      Effect   = signif(df$effect, 4),
      `95% CI` = paste0("(", signif(df$CI_low, 4), ", ", signif(df$CI_high, 4), ")"),
      `p-value` = pfmt,
      VIF      = VIF_col,
      .__sig__  = sig_flag,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(out)[names(out) == "Effect"] <- eff_name
    
    sig_idx <- which(names(out) == ".__sig__") - 1
    
    DT::datatable(
      out,
      options = list(
        scrollX = TRUE, pageLength = 10,
        columnDefs = list(list(visible = FALSE, targets = sig_idx))
      ),
      rownames = FALSE
    ) %>% highlight_sig_rows()
  })
  
  
  output$coef_table <- renderDT({
    df <- coef_table()
    req(!is.null(df), nrow(df) > 0)
    
    eff_name <- if ("OR" %in% df$metric) "OR" else if ("HR" %in% df$metric) "HR" else "\u03B2"
    pnum <- df$p_num
    pfmt <- ifelse(!is.na(pnum) & pnum < 0.001, "<0.001",
                   ifelse(!is.na(pnum), sprintf("%.3f", pnum), as.character(df$p)))
    sig_flag <- ifelse(!is.na(pnum) & pnum < 0.05, "sig", "ns")
    
    n_rows <- nrow(df)
    N_col   <- if ("N"   %in% names(df)) df$N else rep(NA_integer_, n_rows)
    VIF_col <- if ("VIF" %in% names(df)) round(df$VIF, 3) else rep(NA_real_,   n_rows)
    
    out <- data.frame(
      Variable = df$term,
      N        = N_col,
      Effect   = signif(df$effect, 4),
      `95% CI` = paste0("(", signif(df$CI_low, 4), ", ", signif(df$CI_high, 4), ")"),
      `p-value` = pfmt,
      VIF      = VIF_col,
      .__sig__  = sig_flag,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(out)[names(out) == "Effect"] <- eff_name
    
    sig_idx <- which(names(out) == ".__sig__") - 1
    
    DT::datatable(
      out,
      options = list(
        scrollX = TRUE, pageLength = 10,
        columnDefs = list(list(visible = FALSE, targets = sig_idx))
      ),
      rownames = FALSE
    ) %>% highlight_sig_rows()
  })
  
  # --- Diagnostics ---
  # --- REPLACE THIS WHOLE BLOCK ---
  output$model_notes <- renderPrint({
    fit <- fit_store()
    
    # validate() 대신 안전 가드
    if (is.null(fit)) {
      cat("No model fitted yet.")
      return(invisible())
    }
    
    if (inherits(fit, "coxph")) {
      # PH 가정 진단 안전 실행
      tryCatch({
        ph <- survival::cox.zph(fit)
        print(ph)
      }, error = function(e) {
        cat("cox.zph failed:", e$message)
      })
    } else {
      # 일반 회귀 요약
      tryCatch({
        print(summary(fit))
      }, error = function(e) {
        cat("summary failed:", e$message)
      })
    }
  })
  # --- END REPLACE ---
  
  
  # --- Downloads ---
  output$download_table <- downloadHandler(
    filename = function() paste0("model_table_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(coef_table(), file)
    }
  )
  
  output$download_model <- downloadHandler(
    filename = function() paste0("model_object_", Sys.Date(), ".rds"),
    content = function(file) {
      saveRDS(fit_store(), file)
    }
  )
  
  # --- Snapshots ---
  snapshots <- reactiveVal(
    tibble::tibble(
      timestamp = character(),
      label = character(),
      model_type = character(),
      formula = character(),
      term = character(),
      metric = character(),
      effect = double(),
      ci_low = double(),
      ci_high = double(),
      p = double()
    )
  )
  
  output$snapshots <- renderDT({
    datatable(snapshots(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$download_snapshots <- downloadHandler(
    filename = function() paste0("snapshots_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(snapshots(), file)
    }
  )
  
  # --- Reset selections on model_type change (keep front-end behavior) ---
  observeEvent(input$model_type, {
    rv$sel   <- character(0)
    rv$force <- character(0)
    fit_store(NULL)
  })
}

shinyApp(ui, server)
