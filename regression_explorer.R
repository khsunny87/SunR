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
#source("Sun_forest.R")
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


sidebar<-sidebarPanel(width = 4,  # 사이드바 너비
  h4("1) Data"),  # 섹션 제목
      fileInput("file", "Upload CSV (utf-8)", accept = c('.csv', 'text/csv', 'text/plain')),  # CSV 업로드
      prettySwitch("use_example", "Use example data", value = TRUE, status = "info"),  # 예제 데이터 사용 여부
      conditionalPanel(
        condition = "input.use_example == true",  # 예제 사용 시 메시지
        helpText("Examples: mtcars (for linear/logistic), lung (for Cox)")),
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
      #helpText("Selected covariates are listed below. '*' marks force-in."),
      uiOutput("covariate_list"),
      hr(),
      
  actionButton("save_snapshot", "Save Snapshot"),  # 스냅샷 저장 버튼
      textInput("snapshot_label", NULL, placeholder = "Optional label for snapshot"),  # 라벨 붙이기
      downloadButton("download_model", "Download model (.rds)"),  # 모델 저장
      downloadButton("download_table", "Download table (.csv)")   # 결과 테이블 저장
  )

# UI 구성 시작
ui <- fluidPage(
  titlePanel("Interactive Regression Workbench"),  # 앱 상단 제목

  sidebarLayout(sidebar,
    
    mainPanel(
      width = 8,  # 메인 영역 너비
      tabsetPanel(
        tabPanel("Data",  # 데이터 탭: 업로드/예제 데이터 미리보기
                 h5("Preview"),
                 DTOutput("data_preview")),
        tabPanel("Variable selection",  # 단변량 결과 요약 탭
                 helpText("Each candidate covariate is fit in a univariable model with the chosen outcome."),
                 DTOutput("univ_table")),
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
        
        
        #tabPanel("Model result",  # 다변량 모델 결과 탭
        #         h5("Model formula"),
        #         verbatimTextOutput("formula_txt"),
        #         h5("Coefficients"),
        #         DTOutput("coef_table"),
        #         h5("Model diagnostics / notes"),
        #         verbatimTextOutput("model_notes")),
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
  # --- 공통 스타일 함수: P<0.05 행 하이라이트 ---
highlight_sig_rows <- function(dt_widget, sig_col = ".__sig__") {
  DT::formatStyle(
    dt_widget,
    sig_col, target = "row",
    backgroundColor = DT::styleEqual(c("ns","sig"), c(NA, "yellow")),
    color          = DT::styleEqual(c("ns","sig"), c(NA, "red")),
    fontWeight     = DT::styleEqual(c("ns","sig"), c("normal", "bold"))
  )
}

  rv <- reactiveValues(sel = character(0), force = character(0))  # 선택/강제포함 상태 저장
  
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
      req(input$time_col, input$event_col)
      needed <- c(input$time_col, input$event_col, rv$sel, rv$force)
    } else {
      req(input$outcome)
      needed <- c(input$outcome, rv$sel, rv$force)
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
  output$na_info <- renderTable({
    df  <- data_ex()
    df2 <- cleaned_data()
    data.frame(
      Metric = c("Original N", "Cleaned N", "Dropped"),
      Value  = c(nrow(df), nrow(df2), nrow(df) - nrow(df2)),
      check.names = FALSE
    )
  },
  rownames = FALSE,  # 행 이름 숨김
  align    = "lr"    # 왼쪽정렬/오른쪽정렬
  )
  
  # ---------------------
  # (7) 데이터 미리보기 탭 출력
  # ---------------------
  output$data_preview <- renderDT({
    datatable(head(data_ex(), 30), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$covariate_list <- renderUI({
    # rv$sel / rv$force는 네가 앞에서 만든 reactiveValues 상태 사용
    sel <- rv$sel
    force <- rv$force
    
    if (length(sel) == 0) {
      return(tags$p("No covariates selected yet. Use the checkboxes in the 'Variable selection' table."))
    }
    
    items <- vapply(sel, function(v) {
      if (v %in% force) paste0(v, " *") else v
    }, character(1))
    
    tagList(
      tags$ul(lapply(items, function(x) tags$li(x))),
      tags$p(tags$small("* force-in"))
    )
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
      
      n_obs <- tryCatch({
        if (inherits(fit, "coxph")) {
          # Cox PH 모델: coxph 객체는 사용된 관측치 수를 fit$n에 보관함
          as.integer(fit$n)
        } else if (!is.null(fit) && !is.null(fit$model)) {
          # GLM 등: model 프레임이 들어있음
          nrow(fit$model)
        } else if (!is.null(fit) && !is.null(fit$y)) {
          # 예비 안전장치: y가 있으면 그 길이로
          NROW(fit$y)
        } else {
          NA_integer_
        }
      }, error = function(e) NA_integer_)
      if (is.null(fit)) {
        print("❌ 피팅 실패")  # 확인 6
        #return(tibble::tibble(var = v, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
        return(tibble::tibble(var = v, N = n_obs, effect = NA_real_, se = NA_real_, p = NA_real_, note = NA_character_))
        
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
      
      #tibble::tibble(var = v, effect = eff, se = se, p = p, note = note)
      tibble::tibble(var = v, N = n_obs, effect = eff, se = se, p = p, note = note)
      
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
    # (1) 유니버스 결과 가져오기
    dt <- univ_tbl()
    req(!is.null(dt), nrow(dt) > 0)
    
    # (2) 현재 선택 상태(표시 갱신용 의존성)
    sel_now   <- rv$sel
    force_now <- rv$force
    
    # (3) 표시용 데이터 준비 (원래 통계 컬럼 유지)
    base <- dt %>% dplyr::select(var, N, effect, se, p, note)
    
    # (4) 체크박스 열 추가 (escape=FALSE로 렌더)
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
        disabled <- !(v %in% sel_now)  # 선택 안 된 변수 → 비활성
        as.character(shiny::tags$input(
          type = "checkbox", class = "forceChk",
          id = paste0("force_", v),
          checked  = if (v %in% force_now) "checked" else NULL,
          disabled = if (disabled) "disabled" else NULL
        ))
      },
      character(1)
    )
    
    # (5) 보기 좋은 형식(원래 너 포맷 유지) + 시그널 컬럼 추가
    out <- base %>%
      dplyr::mutate(
        CI_low  = ifelse(is.na(effect), NA, effect - 1.96 * se),
        CI_high = ifelse(is.na(effect), NA, effect + 1.96 * se),
        `p-value` = dplyr::case_when(
          is.na(p) ~ NA_character_,
          p < 0.001 ~ "<0.001",
          TRUE ~ sprintf("%.3f", p)
        ),
        # 시그널 플래그 (행 하이라이트용 숨김 컬럼)
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
        .__sig__    # ← 반드시 포함 (숨김용)
      )
    
    # (6) 모델타입에 맞춰 Effect 헤더명 교체 (β / OR / HR)
    eff_name <- switch(input$model_type, "linear"="β", "logistic"="OR", "cox"="HR")
    names(out)[names(out) == "Effect"] <- eff_name
    
    # 숨김 컬럼 인덱스 (DataTables는 0-based)
    sig_idx <- which(names(out) == ".__sig__") - 1
    
    DT::datatable(
      out,
      escape = FALSE, selection = "none", rownames = FALSE,
      options = list(
        scrollX = TRUE, pageLength = 10, ordering = FALSE,
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
    ) %>% highlight_sig_rows()   # ← 여기 추가
    
  })
  
  # Select 체크 변경
  observeEvent(input$sel_changed, {
    v <- input$sel_changed$var
    if (isTRUE(input$sel_changed$checked)) {
      rv$sel <- union(rv$sel, v)
    } else {
      rv$sel   <- setdiff(rv$sel, v)
      rv$force <- setdiff(rv$force, v)  # 선택 해제되면 Force-in도 해제
    }
  }, ignoreInit = TRUE)
  
  # Force-in 체크 변경 (선택된 변수에만 반영)
  observeEvent(input$force_changed, {
    v <- input$force_changed$var
    if (!(v %in% rv$sel)) return()  # 선택 안 된 변수면 무시
    if (isTRUE(input$force_changed$checked)) {
      rv$force <- union(rv$force, v)
    } else {
      rv$force <- setdiff(rv$force, v)
    }
  }, ignoreInit = TRUE)
  
  


  
  # ---------------------
  # (11) 현재 선택된 변수들로 모델 포뮬러 생성
  # ---------------------
  current_formula <- reactive({
    df <- cleaned_data()
    vars <- unique(c(rv$force, rv$sel))
    
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
    no_predictors <- identical(attr(terms(form), "term.labels"), character(0))
    if (input$model_type == "cox" && no_predictors) {
      showNotification("Cox PH requires at least one predictor. Select covariates first.", type = "warning")
      fit_store(NULL)
      return(invisible(NULL))
    }
    base_fit <- NULL
    if (input$model_type == "linear") {
      base_fit <- safe_glm(form, df)
    } else if (input$model_type == "logistic") {
      base_fit <- safe_glm(form, df, family = binomial())
    } else {
      base_fit <- safe_cox(form, df)
    }
    # --- ensure stepAIC/update() sees a real data.frame, not a dead local symbol ---
    df <- as.data.frame(df)
    if (!is.null(base_fit)) {
      base_fit$call$data <- df
    }
    
    
    validate(need(!is.null(base_fit), "Model failed to fit. Check variables and data."))
    
    # --- guard: no predictors (~1) -> skip stepwise ---
    #no_predictors <- identical(attr(terms(form), "term.labels"), character(0))
    
    if (input$step_direction != "none" && !no_predictors) {
      # ---- force-in 보장: stepAIC scope 설정 ----
      force_vars <- rv$force
      all_vars   <- unique(c(rv$force, rv$sel))
      
      # LHS 구성
      lhs <- if (input$model_type == "cox") {
        paste0("Surv(", input$time_col, ", ", input$event_col, ")")
      } else {
        input$outcome
      }
      
      # lower: force-in만(없으면 ~1)
      lower_rhs <- if (length(force_vars) > 0) paste(force_vars, collapse = "+") else "1"
      # upper: force-in + 선택변수 전체(없으면 ~1)
      upper_rhs <- if (length(all_vars) > 0) paste(all_vars, collapse = "+") else "1"
      
      lower_form <- as.formula(paste(lhs, "~", lower_rhs))
      upper_form <- as.formula(paste(lhs, "~", upper_rhs))
      
      k_val <- if (input$criterion == "AIC") 2 else log(nrow(df))  # BIC: k = log(n)
      
      step_fit <- stepAIC(
        base_fit,
        scope     = list(lower = lower_form, upper = upper_form),
        direction = input$step_direction,
        k         = k_val,
        trace     = FALSE
      )
      
      fit_store(step_fit)
    } else {
      fit_store(base_fit)
    }
    
    
    
  })
  
  
  # ---------------------
  # (14) 회귀 계수 테이블 생성 (conf.int 포함, 변환도 포함)
  # ---------------------

  coef_table <- reactive({
    fit <- fit_store()
    validate(need(!is.null(fit), ""))
    n_obs <- tryCatch({
      if (inherits(fit, "coxph")) {
        as.integer(fit$n)              # coxph는 fit$n에 N 저장
      } else if (!is.null(fit)) {
        as.integer(stats::nobs(fit))   # glm 등은 nobs로 충분
      } else {
        NA_integer_
      }
    }, error = function(e) NA_integer_)
    
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
    # Format p-values (표시는 문자열, 판정용은 숫자 보존)
    tdy <- tdy %>%
      dplyr::mutate(
        p_num = p.value,
        p     = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
      )
    
    tdy <- tdy %>%
      dplyr::mutate(N = n_obs) %>%        # N을 모든 행에 부여
      dplyr::select(term, metric, effect, CI_low, CI_high, p_num, p, N)
    
  })
  
  # ---------------------
  # (15) Coefficients 테이블 UI 출력
  # ---------------------
  # (15) Coefficients 테이블 UI 출력  ← 이 블록만 교체
  # (15) Coefficients 테이블 UI 출력  ← 이 블록만 교체
  

 # (15) Coefficients 테이블 UI 출력  ← 이 블록만 교체

output$coef_table <- renderDT({
  df <- coef_table()
  req(!is.null(df), nrow(df) > 0)

  # 헤더명(β/OR/HR) 결정 - metric만 보고 정함
  eff_name <- if ("OR" %in% df$metric) "OR" else if ("HR" %in% df$metric) "HR" else "\u03B2"

  # ✅ 숫자 p값은 df$p_num을 그대로 사용 (문자열 df$p 변환 금지)
  pnum <- df$p_num

  # 표시용 p값 문자열은 df$p를 기본으로 하되, 숫자일 때는 포맷統一
  pfmt <- ifelse(!is.na(pnum) & pnum < 0.001, "<0.001",
                 ifelse(!is.na(pnum), sprintf("%.3f", pnum), as.character(df$p)))

  # ✅ 하이라이트 플래그도 df$p_num 기준
  sig_flag <- ifelse(!is.na(pnum) & pnum < 0.05, "sig", "ns")

  # 출력용 테이블 (+ 숨김 컬럼)
  out <- data.frame(
    Variable = df$term,
    N        = df$N,  # ← 새로 추가된 N 컬럼
    Effect   = signif(df$effect, 4),
    `95% CI` = paste0("(", signif(df$CI_low, 4), ", ", signif(df$CI_high, 4), ")"),
    `p-value` = pfmt,
    .__sig__  = sig_flag,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "Effect"] <- eff_name

  # 숨김 컬럼 인덱스 (0-based)
  sig_idx <- which(names(out) == ".__sig__") - 1

  DT::datatable(
    out,
    options = list(
      scrollX = TRUE, pageLength = 10,
      columnDefs = list(list(visible = FALSE, targets = sig_idx))
    ),
    rownames = FALSE
  ) %>% highlight_sig_rows()   # ← 여기 추가
})
 

  
  
  # ---------------------
  # (16) Diagnostics 출력 (cox.zph 또는 summary)
  # ---------------------
  output$model_notes <- renderPrint({
    fit <- fit_store()
    validate(need(!is.null(fit), "")) 
    
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

  # 모델 타입 바뀌면 선택/강제포함/모델 초기화
  observeEvent(input$model_type, {
    rv$sel   <- character(0)
    rv$force <- character(0)
    if (exists("fit_store")) fit_store(NULL)  # 네가 fit_store() 쓰는 경우만 유지. 아니면 이 줄 지워.
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
