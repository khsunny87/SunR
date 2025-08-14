# app.R
# ---------------------------------------------
# Interactive Regression Workbench
# - Upload data
# - Choose outcome & model type (Linear / Logistic / Cox PH)
# - See univariable estimates next to each covariate to guide selection
# - Clean data (complete-case on selected variables)
# - Optional stepwise selection (AIC or BIC; forward/backward/both)
# - Inspect results, adjust interactively, and save snapshots
# ---------------------------------------------
# ---------------------------------------------
# Interactive Regression Workbench - 완전 주석 버전
# ---------------------------------------------
library(dplyr)           # 데이터 처리 (filter, mutate 등)

# 패키지 로딩 시 나오는 메시지(예: Masked functions 등)를 숨김
suppressPackageStartupMessages({
  library(shiny)           # Shiny 웹앱 프레임워크
  library(shinyWidgets)    # 더 다양한 UI 위젯들 제공
  library(DT)              # HTML 테이블 출력용 (DataTable)
  library(tidyr)           # 데이터 재구조화 (pivot, nest 등)
  library(stringr)         # 문자열 처리 함수들
  library(readr)           # 빠른 CSV 읽기/쓰기
  library(broom)           # 회귀 결과 정리 (tidy, glance 등)
  library(MASS)            # stepAIC 함수 사용 (stepwise regression)
  library(survival)        # Cox 모델, Surv() 등 생존분석 함수들
})

# UI 구성 시작
ui <- fluidPage(
  titlePanel("Interactive Regression Workbench"),  # 앱 상단 제목
  
  sidebarLayout(
    sidebarPanel(
      width = 4,  # 사이드바 너비
      
      h4("1) Data"),  # 섹션 제목
      fileInput("file", "Upload CSV (utf-8)", accept = c('.csv', 'text/csv', 'text/plain')),  # CSV 업로드
      prettySwitch("use_example", "Use example data", value = TRUE, status = "info"),  # 예제 데이터 사용 여부
      conditionalPanel(
        condition = "input.use_example == true",  # 예제 사용 시 메시지
        helpText("Examples: mtcars (for linear/logistic), lung (for Cox)")
      ),
      hr(),  # 구분선
      
      h4("2) Outcome & Model"),  # 섹션 제목: 종속변수 및 모델 종류
      selectInput("model_type", "Model type",  # 모델 유형 선택
                  choices = c("Linear (Gaussian)" = "linear",
                              "Logistic (Binomial)" = "logistic",
                              "Cox PH (Survival)" = "cox"), 
                  selected = "linear"),
      
      uiOutput("outcome_ui"),  # 선형/로지스틱 회귀용 종속변수 선택 UI
      uiOutput("surv_ui"),     # Cox 모델용 시간/사건 변수 선택 UI
      hr(),
      
      h4("3) Candidate Covariates"),  # 섹션 제목: 후보 독립변수들
      helpText("Univariable estimates are shown to help choose covariates for multivariable model."),
      pickerInput(  # 공변량 선택
        inputId = "covars",
        label = "Select covariates (hold Ctrl/Cmd for multi)",
        choices = NULL,
        multiple = TRUE,
        options = list(`live-search` = TRUE, size = 10)
      ),
      checkboxGroupInput("force_in", "Force-include variables (always kept)", choices = NULL),  # 항상 포함할 변수 지정
      hr(),
      
      h4("4) Cleaning"),  # 섹션 제목: 결측값 처리
      checkboxInput("drop_na", "Complete-case analysis (drop rows with missing in selected vars)", TRUE),
      verbatimTextOutput("na_info"),  # 결측값 정보 출력
      hr(),
      
      h4("5) Stepwise (optional)"),  # 섹션 제목: Stepwise 선택
      prettyRadioButtons("step_direction", "Direction",  # 방향 선택
                         choices = c("none", "forward", "backward", "both"),
                         selected = "none", inline = TRUE),
      prettyRadioButtons("criterion", "Criterion",       # AIC 또는 BIC 선택
                         choices = c("AIC", "BIC"), 
                         selected = "AIC", inline = TRUE),
      
      hr(),
      
      actionButton("fit", "Fit Model", class = "btn-primary"),  # 모델 적합 버튼
      br(), br(),
      actionButton("save_snapshot", "Save Snapshot"),  # 스냅샷 저장 버튼
      textInput("snapshot_label", NULL, placeholder = "Optional label for snapshot"),  # 라벨 붙이기
      downloadButton("download_model", "Download model (.rds)"),  # 모델 저장
      downloadButton("download_table", "Download table (.csv)")   # 결과 테이블 저장
    ),
    
    mainPanel(
      width = 8,  # 메인 영역 너비
      tabsetPanel(
        tabPanel("Data",  # 데이터 탭: 업로드/예제 데이터 미리보기
                 h5("Preview"),
                 DTOutput("data_preview")),
        tabPanel("Univariable screening",  # 단변량 결과 요약 탭
                 helpText("Each candidate covariate is fit in a univariable model with the chosen outcome."),
                 DTOutput("univ_table")),
        tabPanel("Model result",  # 다변량 모델 결과 탭
                 h5("Model formula"),
                 verbatimTextOutput("formula_txt"),
                 h5("Coefficients"),
                 DTOutput("coef_table"),
                 h5("Model diagnostics / notes"),
                 verbatimTextOutput("model_notes")),
        tabPanel("Saved results",  # 저장된 결과 스냅샷 확인
                 DTOutput("snapshots"),
                 downloadButton("download_snapshots", "Download all snapshots (.csv)"))
      )
    )
  )
)


# ---------------------
# 서버 로직 시작
# ---------------------
server <- function(input, output, session) {
  fit_store <- reactiveVal(NULL)   # 현재 적합된 멀티변수 모델을 보관/초기화용
  
  # ---------------------
  # (1) 예제 데이터 정의
  # ---------------------
  data_ex <- reactive({
    if (isTRUE(input$use_example)) {
      # Cox 모델을 선택한 경우 -> survival 패키지의 lung 데이터 사용
      if (input$model_type == "cox") {
        lung %>% mutate(status = ifelse(status == 2, 1, 0))  # 1=event, 0=censor로 변환
      } else {
        # 선형/로지스틱 모델의 경우 -> mtcars 데이터 사용
        mtcars %>% mutate(across(where(is.numeric), as.numeric))  # 숫자형 유지
      }
    } else {
      # 사용자 업로드 파일 사용 시
      req(input$file)  # 파일이 반드시 있어야 함
      read_csv(input$file$datapath, show_col_types = FALSE)  # CSV 읽기
    }
  })
  
  # ---------------------
  # (2) 종속변수 선택 UI 출력 (선형/로지스틱만)
  # ---------------------
  output$outcome_ui <- renderUI({
    df <- data_ex()
    cols <- names(df)
    if (input$model_type == "cox") return(NULL)  # Cox에서는 종속변수 UI 없음
    selectInput("outcome", "Outcome variable", choices = cols)
  })
  
  # ---------------------
  # (3) 생존시간 + 이벤트 선택 UI 출력 (Cox 모델일 때만)
  # ---------------------
  output$surv_ui <- renderUI({
    if (input$model_type != "cox") return(NULL)
    df <- data_ex()
    cols <- names(df)
    tagList(
      selectInput("time_col", "Survival time column", choices = cols),         # 생존시간 컬럼
      selectInput("event_col", "Event indicator column (0/1)", choices = cols),# 이벤트 컬럼 (0/1)
      checkboxInput("nonzero_event", "Treat non-zero as event (coerce to 0/1)", TRUE)  # 비0 → 1로 처리
    )
  })
  
  # ---------------------
  # (4) 모델 적합 시 사용하는 안전한 wrapper 함수들
  # ---------------------
  safe_glm <- function(form, data, family = NULL) {
    out <- tryCatch({
      if (is.null(family)) glm(form, data = data) else glm(form, data = data, family = family)
    }, error = function(e) NULL)  # 에러 시 NULL 리턴
    out
  }
  
  safe_cox <- function(form, data) {
    out <- tryCatch({
      coxph(form, data = data)
    }, error = function(e) NULL)
    out
  }
  
  # ---------------------
  # (5) 데이터 전처리 (결측치 제거 및 이벤트 변환 등)
  # ---------------------
  cleaned_data <- reactive({
    df <- data_ex()  # 원본 데이터 불러오기
    
    # 분석에 필요한 변수 수집
    needed <- character(0)
    if (input$model_type == "cox") {
      req(input$time_col, input$event_col)  # 둘 다 선택되어야 함
      needed <- c(input$time_col, input$event_col, input$covars, input$force_in)
    } else {
      req(input$outcome)  # outcome 필수
      needed <- c(input$outcome, input$covars, input$force_in)
    }
    needed <- unique(needed[needed != ""])  # 공백 제거 및 중복 제거
    
    df2 <- df  # 복사본 만들기
    
    # 이벤트 변수를 0/1로 변환 (옵션이 체크된 경우)
    if (input$model_type == "cox" && !is.null(input$event_col)) {
      if (isTRUE(input$nonzero_event)) {
        df2[[input$event_col]] <- as.integer(df2[[input$event_col]] != 0)
      }
    }
    
    # 결측값 제거 (drop_na가 체크된 경우)
    if (isTRUE(input$drop_na) && length(needed) > 0) {
      df2 <- df2[complete.cases(df2[, needed, drop = FALSE]), ]
    }
    
    df2  # 전처리된 데이터 반환
  })
  
  # ---------------------
  # (6) 전처리 전후 비교 정보 출력
  # ---------------------
  output$na_info <- renderPrint({
    df <- data_ex()
    df2 <- cleaned_data()
    list(
      original_n = nrow(df),      # 원본 데이터 행 수
      cleaned_n  = nrow(df2),     # 결측 제거 후 행 수
      dropped    = nrow(df) - nrow(df2)  # 제거된 행 수
    )
  })
  
  # ---------------------
  # (7) 데이터 미리보기 탭 출력
  # ---------------------
  output$data_preview <- renderDT({
    datatable(head(data_ex(), 30), options = list(scrollX = TRUE, pageLength = 10))
  })
  # ---------------------
  # (8) Univariable screening 테이블 계산
  # ---------------------
  univ_tbl <- reactive({
    print("💥 univ_tbl() 시작됨")  # 확인 1
    
    df <- cleaned_data()
    print(paste("🔢 cleaned_data 행 수:", nrow(df)))  # 확인 2
    
    if (input$model_type == "cox") {
      print("🧪 모델타입: COX")
      drop_cols <- c(input$time_col, input$event_col)
    } else {
      print(paste("🧪 모델타입:", input$model_type))
      drop_cols <- input$outcome
    }
    print(paste("📌 제외할 변수들:", paste(drop_cols, collapse = ", ")))  # 확인 3
    
    cand <- setdiff(names(df), drop_cols)
    print(paste("📌 후보 변수들:", paste(cand, collapse = ", ")))  # 확인 4
    
    res <- lapply(cand, function(v) {
      print(paste("⚙️ 처리 중 변수:", v))  # 확인 5
      
      f <- switch(input$model_type,
                  linear   = as.formula(paste0(input$outcome, " ~ ", v)),
                  logistic = as.formula(paste0(input$outcome, " ~ ", v)),
                  cox      = as.formula(paste0("Surv(", input$time_col, ", ", input$event_col, ") ~ ", v)))
      
      fit <- switch(input$model_type,
                    linear   = safe_glm(f, df),
                    logistic = safe_glm(f, df, family = binomial()),
                    cox      = safe_cox(f, df))
      
      if (is.null(fit)) {
        print("❌ 피팅 실패")  # 확인 6
        return(tibble::tibble(var = v, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
      }
      
      tdy <- tryCatch(broom::tidy(fit), error = function(e) {
        print(paste("❌ tidy 실패:", e$message))
        return(NULL)
      })
      if (is.null(tdy) || !"estimate" %in% names(tdy)) {
        print("❌ tidy 결과 없음 또는 estimate 없음")
        return(tibble::tibble(var = v, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
      }
      
      tdy <- tdy %>% filter(term != "(Intercept)")
      if (nrow(tdy) == 0) {
        print("❌ term이 전부 Intercept라 결과 없음")
        return(tibble::tibble(var = v, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
      }
      
      est <- tdy$estimate[1]
      se  <- tdy$std.error[1]
      p   <- tdy$p.value[1]
      note <- switch(input$model_type, linear = "Beta", logistic = "OR", cox = "HR")
      eff <- if (note %in% c("OR", "HR")) exp(est) else est
      
      tibble::tibble(var = v, effect = eff, se = se, p = p, note = note)
    })
    
    print("✅ lapply 종료. bind_rows 실행")  # 확인 7
    final <- purrr::compact(res) %>% bind_rows()
    print("✅ 최종 테이블 생성 완료")         # 확인 8
    final
  })
  
  
  # ---------------------
  # (9) Univariable 결과 테이블 출력 (Univariable screening 탭)
  # ---------------------
  output$univ_table <- renderDT({
    print("🔥 renderDT: univ_table 호출됨")
    dt <- univ_tbl()
    
    if (nrow(dt) == 0 || !all(c("var", "effect", "se", "p", "note") %in% names(dt))) {
      return(datatable(data.frame()))
    }
    
    dt_clean <- dt %>%
      mutate(
        CI_low  = effect - 1.96 * se,
        CI_high = effect + 1.96 * se,
        CI_low  = ifelse(is.na(effect), NA, CI_low),
        CI_high = ifelse(is.na(effect), NA, CI_high),
        p_fmt = ifelse(p < 0.001, "<0.001", format(round(p, 3), nsmall = 3))
      ) %>%
      transmute(
        Variable = var,
        Effect   = round(effect, 4),
        `95% CI` = paste0("(", round(CI_low, 4), ", ", round(CI_high, 4), ")"),
        `p-value` = p_fmt
      )
    
    # 모델 타입에 따라 헤더 이름 설정
    effect_name <- switch(input$model_type,
                          "linear"   = "β",
                          "logistic" = "OR",
                          "cox"      = "HR")
    
    colnames(dt_clean)[which(names(dt_clean) == "Effect")] <- effect_name
    
    datatable(dt_clean, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  
  # ---------------------
  # (10) covariate 선택 창 업데이트 (결과값 포함된 라벨로 동기화)
  # ---------------------
 

observe({
  dt <- univ_tbl()
  choices <- dt$var
  names(choices) <- choices
  
  
  updatePickerInput(session, "covars", choices = choices, selected = input$covars)
  updateCheckboxGroupInput(session, "force_in", choices = dt$var, selected = input$force_in)
})

  
  # ---------------------
  # (11) 현재 선택된 변수들로 모델 포뮬러 생성
  # ---------------------
  current_formula <- reactive({
    df <- cleaned_data()
    vars <- unique(c(input$force_in, input$covars))  # 포함된 모든 변수들
    
    if (input$model_type == "linear") {
      req(input$outcome)
      if (length(vars) == 0) {
        # 변수 없으면 단순 절편 모델
        as.formula(paste0(input$outcome, " ~ 1"))
      } else {
        # 다변량 모델 포뮬러 생성
        as.formula(paste0(input$outcome, " ~ ", paste(vars, collapse = "+")))
      }
      
    } else if (input$model_type == "logistic") {
      req(input$outcome)
      if (length(vars) == 0) {
        as.formula(paste0(input$outcome, " ~ 1"))
      } else {
        as.formula(paste0(input$outcome, " ~ ", paste(vars, collapse = "+")))
      }
      
    } else {
      # Cox 모델의 경우
      req(input$time_col, input$event_col)
      lhs <- paste0("Surv(", input$time_col, ", ", input$event_col, ")")
      if (length(vars) == 0) {
        as.formula(paste0(lhs, " ~ 1"))
      } else {
        as.formula(paste0(lhs, " ~ ", paste(vars, collapse = "+")))
      }
    }
  })
  
  # ---------------------
  # (12) UI에 모델 포뮬러 출력
  # ---------------------
  output$formula_txt <- renderPrint({
    current_formula()
  })
  
  # ---------------------
  # (13) Fit Model 버튼 누르면 모델 적합 수행 (선택에 따라 stepwise 포함)
  # ---------------------
  
  # ---------------------
  # (13) Fit Model 버튼 → 모델 적합 후 fit_store에 저장
  # ---------------------
  observeEvent(input$fit, {
    df <- cleaned_data()
    form <- current_formula()
    
    base_fit <- NULL
    if (input$model_type == "linear") {
      base_fit <- safe_glm(form, df)
    } else if (input$model_type == "logistic") {
      base_fit <- safe_glm(form, df, family = binomial())
    } else {
      base_fit <- safe_cox(form, df)
    }
    
    validate(need(!is.null(base_fit), "Model failed to fit. Check variables and data."))
    
    if (input$step_direction != "none") {
      k_val <- if (input$criterion == "AIC") 2 else log(nrow(df))  # BIC: k = log(n)
      step_fit <- stepAIC(base_fit, direction = input$step_direction, k = k_val, trace = FALSE)
      fit_store(step_fit)     # ← 저장
    } else {
      fit_store(base_fit)     # ← 저장
    }
  })
  
  
  # ---------------------
  # (14) 회귀 계수 테이블 생성 (conf.int 포함, 변환도 포함)
  # ---------------------

  coef_table <- reactive({
    fit <- fit_store()
    df <- cleaned_data()
    tdy <- broom::tidy(fit, conf.int = TRUE, conf.level = 0.95)
    tdy <- tdy %>% dplyr::filter(term != "(Intercept)")
    
    # Metric-specific transformation
    if (inherits(fit, "coxph")) {
      tdy <- tdy %>%
        dplyr::mutate(
          metric = "HR",
          effect = exp(estimate),
          CI_low = exp(conf.low),
          CI_high = exp(conf.high)
        )
    } else if (!is.null(family(fit)) && family(fit)$family == "binomial") {
      tdy <- tdy %>%
        dplyr::mutate(
          metric = "OR",
          effect = exp(estimate),
          CI_low = exp(conf.low),
          CI_high = exp(conf.high)
        )
    } else {
      tdy <- tdy %>%
        dplyr::mutate(
          metric = "Beta",
          effect = estimate,
          CI_low = conf.low,
          CI_high = conf.high
        )
    }
    
    # Format p-values
    tdy <- tdy %>%
      dplyr::mutate(
        p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
      )
    
    tdy %>% dplyr::select(term, metric, effect, CI_low, CI_high, p)
  })
  
  # ---------------------
  # (15) Coefficients 테이블 UI 출력
  # ---------------------
  # (15) Coefficients 테이블 UI 출력  ← 이 블록만 교체
  output$coef_table <- renderDT({
    df <- coef_table()
    req(!is.null(df), nrow(df) > 0)
    
    # 헤더명(β/OR/HR) 결정 - metric만 보고 정함
    eff_name <- if ("OR" %in% df$metric) "OR" else if ("HR" %in% df$metric) "HR" else "\u03B2"
    
    # p 포맷 통일 (숫자/문자 혼재 방어)
    pnum <- suppressWarnings(as.numeric(df$p))
    pfmt <- ifelse(!is.na(pnum) & pnum < 0.001, "<0.001",
                   ifelse(!is.na(pnum), sprintf("%.3f", pnum), as.character(df$p)))
    
    # 출력용 테이블 (표현만 변경, 계산 없음)
    out <- data.frame(
      Variable = df$term,
      Effect   = signif(df$effect, 4),
      `95% CI` = paste0("(", signif(df$CI_low, 4), ", ", signif(df$CI_high, 4), ")"),
      `p-value` = pfmt,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(out)[names(out) == "Effect"] <- eff_name
    
    datatable(out, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  
  # ---------------------
  # (16) Diagnostics 출력 (cox.zph 또는 summary)
  # ---------------------
  output$model_notes <- renderPrint({
    fit <- fit_store()
    if (inherits(fit, "coxph")) {
      # Cox 모델은 proportional hazards 검정
      tryCatch({
        ph <- survival::cox.zph(fit)
        print(ph)
      }, error = function(e) {
        cat("cox.zph failed:", e$message)
      })
    } else {
      # 일반 회귀는 summary
      print(summary(fit))
    }
  })
  
  # ---------------------
  # (17) 결과 테이블 다운로드 (.csv)
  # ---------------------
  output$download_table <- downloadHandler(
    filename = function() paste0("model_table_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(coef_table(), file)  # readr::write_csv
    }
  )
  
  # ---------------------
  # (18) 모델 객체 다운로드 (.rds)
  # ---------------------
  output$download_model <- downloadHandler(
    filename = function() paste0("model_object_", Sys.Date(), ".rds"),
    content = function(file) {
      saveRDS(fit_store(), file)
    }
  )
  
  # ---------------------
  # (19) Snapshot 저장용 리액티브 값 초기화
  # ---------------------
  snapshots <- reactiveVal(
    tibble::tibble(
      timestamp = character(),  # 저장 시각
      label = character(),      # 사용자가 입력한 라벨
      model_type = character(), # 모델 유형
      formula = character(),    # 회귀식
      term = character(),       # 변수명
      metric = character(),     # HR / OR / Beta
      effect = double(),        # 계수 추정치
      ci_low = double(),        # 95% CI 하한
      ci_high = double(),       # 95% CI 상한
      p = double()              # p값
    )
  )
  
  # ---------------------
  # (20) Snapshot 저장 버튼 누를 때 현재 결과 저장
  # ---------------------

  # 1. 모델 변경 시: choices만 갱신 + 선택값은 초기화
  # 1. 모델 변경 시: choices 갱신 + 선택값 초기화 + 기존 모델 초기화
  observeEvent(input$model_type, {
    dt <- univ_tbl()
    
    # 모델은 무조건 초기화
    fit_store(NULL)   # ← 기존 멀티변수 모델 삭제
    
    # univ_tbl이 비었으면 입력들도 비워주고 종료
    if (is.null(dt) || nrow(dt) == 0 || !all(c("var","label") %in% names(dt))) {
      updatePickerInput(session, "covars", choices = character(0), selected = character(0))
      updateCheckboxGroupInput(session, "force_in", choices = character(0), selected = character(0))
      return()
    }
    
    choices <- dt$var
    labels  <- dt$label
    labels[is.na(labels)] <- choices[is.na(labels)]
    names(choices) <- labels
    
    updatePickerInput(session, "covars", choices = choices, selected = character(0))
    updateCheckboxGroupInput(session, "force_in", choices = dt$var, selected = character(0))
  })
  
  # ---------------------
  # (21) Saved results 탭에 Snapshot 테이블 출력
  # ---------------------
  output$snapshots <- renderDT({
    datatable(snapshots(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # ---------------------
  # (22) 모든 Snapshot 다운로드 (.csv)
  # ---------------------
  output$download_snapshots <- downloadHandler(
    filename = function() paste0("snapshots_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(snapshots(), file)
    }
  )
}

# ---------------------
# (23) 앱 실행
# ---------------------
shinyApp(ui, server)
