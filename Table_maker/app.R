# app.R
# ------------------------------------------------------------
# Variable Picker for Publication Tables (inline checkboxes in table)
# - Load CSV or JSON (or example data)
# - Pick a grouping variable
# - Variable Selection tab: gtsummary 표의 변수명 옆에 체크박스 삽입
# - Final Table tab: 체크된 변수만 깔끔 요약
# ------------------------------------------------------------
options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(DT)
  library(gtsummary)
  library(gt)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(rlang)
  library(shinyjs)
})

# ----- Helpers -----

get_group_candidates <- function(df) {
  if (is.null(df)) return(character(0))
  cls <- vapply(df, function(x) class(x)[1], character(1))
  low_card_num <- names(which(vapply(df, function(x) is.numeric(x) && n_distinct(x, na.rm = TRUE) <= 10, logical(1))))
  cand <- union(names(cls[cls %in% c("factor", "character", "logical")]), low_card_num)
  cand[cand %in% names(df)]
}

coerce_group_factor <- function(df, group_var) {
  if (is.null(df) || !group_var %in% names(df)) return(df)
  if (!is.factor(df[[group_var]])) df[[group_var]] <- as.factor(df[[group_var]])
  df
}

# ID-safe 변환 (input id용)
id_for_var <- function(v) paste0("pick__", gsub("[^A-Za-z0-9_]", "_", v))

# gtsummary 생성 (include_vars=NULL이면 전체)

# ---- build_tbl: 통째로 교체 ----
# ---- build_tbl: 통째로 교체 (show_overall 지원) ----
# ---- build_tbl: (show_overall, show_n 지원) ----
build_tbl <- function(df, group_var, include_vars = NULL,
                      show_overall = FALSE, show_n = FALSE) {
  req(df)
  
  # 1) 그룹 변수 정리
  if (!is.null(group_var) && group_var %in% names(df)) {
    df <- coerce_group_factor(df, group_var)
  } else {
    group_var <- NULL
  }
  
  # 2) 서브셋 (선택 변수만) — 그룹변수는 항상 보존
  if (!is.null(include_vars)) {
    include_vars <- intersect(include_vars, names(df))
    shiny::validate(shiny::need(length(include_vars) > 0, "선택된 변수가 없습니다."))
    keep <- unique(c(group_var, include_vars))
    df <- df[, keep, drop = FALSE]
  }
  
  # 3) 변수 분류: 숫자형 & 고유값 > 10만 연속, 나머지는 범주
  vars_all <- setdiff(names(df), group_var)
  is_num   <- vapply(df[vars_all], is.numeric, logical(1))
  nuniq    <- vapply(df[vars_all], function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1))
  conts    <- vars_all[ is_num & (nuniq > 10) ]
  cats     <- setdiff(vars_all, conts)
  
  # 4) 정규성 (Shapiro-Wilk) — 그룹별 모두 정규면 정규로 간주
  compute_normality <- function(df2, group_var2 = NULL, alpha = 0.05) {
    if (!length(conts)) return(setNames(logical(0), character(0)))
    is_ok <- function(x) {
      x <- x[is.finite(x)]; n <- length(x)
      if (n < 3) return(NA)
      if (n > 5000) x <- sample(x, 5000)
      p <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
      if (is.na(p)) NA else (p >= alpha)
    }
    res <- setNames(rep(FALSE, length(conts)), conts)
    if (!is.null(group_var2)) {
      g <- as.factor(df2[[group_var2]])
      for (v in conts) {
        by_ok <- tapply(df2[[v]], g, is_ok, simplify = TRUE)
        res[[v]] <- all(by_ok[!is.na(by_ok)])
      }
    } else {
      for (v in conts) res[[v]] <- isTRUE(is_ok(df2[[v]]))
    }
    res
  }
  normals <- compute_normality(df, group_var)
  is_norm <- setNames(rep(FALSE, length(conts)), conts)
  if (length(normals)) is_norm[names(normals)] <- as.logical(normals[names(normals)])
  
  # 5) 통계 서식
  stat_list <- list()
  if (length(cats)) {
    stat_list <- append(stat_list, list(gtsummary::all_categorical() ~ "{n} ({p}%)"))
  }
  if (length(conts)) {
    for (v in conts) {
      if (isTRUE(is_norm[[v]])) {
        stat_list <- append(stat_list, setNames(list("{mean} \u00B1 {sd}"), v))
      } else {
        stat_list <- append(stat_list, setNames(list("{median} ({p25}, {p75})"), v))
      }
    }
  }
  
  # 6) 검정
  test_list <- NULL
  if (!is.null(group_var)) {
    tmp <- list()
    if (length(cats)) tmp <- append(tmp, list(gtsummary::all_categorical() ~ "chisq.test"))
    if (length(conts)) {
      for (v in conts) {
        if (isTRUE(is_norm[[v]])) tmp <- append(tmp, setNames(list("t.test"), v))
        else                      tmp <- append(tmp, setNames(list("wilcox.test"), v))
      }
    }
    test_list <- tmp
  }
  
  # 7) gtsummary 생성 + 옵션 적용
  if (is.null(group_var)) {
    tbl <- gtsummary::tbl_summary(
      df,
      statistic = stat_list,
      missing = "ifany"
    )
    if (isTRUE(show_n)) tbl <- tbl |> gtsummary::add_n()
    tbl |>
      gtsummary::modify_header(label ~ "**Variable**") |>
      gtsummary::bold_labels()
  } else {
    shiny::validate(shiny::need(nlevels(df[[group_var]]) >= 2,
                  sprintf("그룹 변수 '%s'의 레벨이 2개 이상이어야 합니다.", group_var)))
    tbl <- gtsummary::tbl_summary(
      df, by = rlang::sym(group_var),
      statistic = stat_list,
      missing = "ifany"
    )
    if (isTRUE(show_overall)) tbl <- tbl |> gtsummary::add_overall(last = FALSE)
    if (isTRUE(show_n))       tbl <- tbl |> gtsummary::add_n()
    tbl |>
      gtsummary::add_p(test = test_list) |>
      gtsummary::modify_header(label ~ "**Variable**") |>
      gtsummary::bold_labels()
  }
}

# ---- build_tbl 끝 ----

# ---- build_tbl 끝 ----


# 현재 상태 JSON 직렬화
state_to_json <- function(df, group_var, selected_vars) {
  toJSON(
    list(
      raw_data      = df,
      group_var     = group_var %||% "",
      selected_vars = selected_vars %||% character(0)
    ),
    dataframe = "rows", pretty = TRUE, na = "null", auto_unbox = TRUE
  )
}

# JSON 로드
state_from_json <- function(path) {
  parsed <- fromJSON(path, simplifyDataFrame = TRUE)
  df <- NULL
  if (!is.null(parsed$raw_data)) df <- as.data.frame(parsed$raw_data, stringsAsFactors = FALSE)
  list(
    df = df,
    group_var = parsed$group_var %||% "",
    selected_vars = parsed$selected_vars %||% character(0)
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

example_datasets <- c("gtsummary::trial", "mtcars")
# 연속변수 정규성 판단: TRUE=정규, FALSE=비정규
compute_normality <- function(df, group_var = NULL, alpha = 0.05) {
  if (is.null(df) || !nrow(df)) return(setNames(logical(0), character(0)))
  is_num <- vapply(df, is.numeric, logical(1))
  conts  <- names(df)[is_num]
  if (length(conts) == 0) return(setNames(logical(0), character(0)))
  
  is_ok_shapiro <- function(x) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n < 3) return(NA)                     # 표본이 너무 작으면 판단 보류
    if (n > 5000) x <- sample(x, 5000)        # Shapiro 제한 회피
    p <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    if (is.na(p)) NA else (p >= alpha)
  }
  
  res <- logical(length(conts))
  names(res) <- conts
  
  if (!is.null(group_var) && group_var %in% names(df)) {
    g <- as.factor(df[[group_var]])
    for (v in conts) {
      by_ok <- tapply(df[[v]], g, is_ok_shapiro, simplify = TRUE)
      # 모든 그룹에서 정규(NA 제외)가 TRUE여야 정규로 간주
      if (all(by_ok[!is.na(by_ok)])) res[v] <- TRUE else res[v] <- FALSE
    }
  } else {
    for (v in conts) res[v] <- is_ok_shapiro(df[[v]]) %||% FALSE
  }
  res
}

# ----- UI -----

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Variable Picker for Publication Tables"),
  sidebarLayout(
    sidebarPanel(
      h4("1) 데이터 불러오기"),
      fileInput("file_csv", "CSV 업로드", accept = c(".csv")),
      fileInput("file_json", "JSON 업로드(상태+데이터)", accept = c(".json")),
      pickerInput(
        "example_data", "또는 예제 데이터",
        choices = example_datasets,
        selected = example_datasets[1],        # ← 추가: 기본값 gtsummary::trial
        multiple = FALSE, options = list(size = 6)),
      br(),
      h4("2) 그룹 변수"),
      uiOutput("ui_group_var"),
      helpText("문자/범주형/저카디널리티 숫자(≤10레벨) 열을 제안합니다."),
      checkboxInput("show_overall", "Overall 열 추가", value = FALSE),
      checkboxInput("show_n", "Add N 표시", value = FALSE),
      
      
      br(),
      h4("3) 현재 상태 저장"),
      downloadButton("download_state", "현재 상태(JSON) 저장"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Variable Selection",
          br(),
          helpText("아래 표에서 변수명 좌측 체크박스를 켜면 선택됩니다."),
          uiOutput("gt_all")

          #gt_output("gt_all")  # 테이블 내부에 체크박스 삽입
        ),
        tabPanel(
          title = "Final Table",
          br(),
          helpText("체크된 변수들만 그룹 비교하여 출력합니다."),
          uiOutput("gt_final")

          #gt_output("gt_final")
        )
      ),
      width = 9
    )
  )
)

# ----- Server -----

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    df = NULL,
    group_var = NULL,
    selected_vars = character(0),
    var_order = character(0),   # << 추가: 변수 표시/최종표 출력 순서
    loading = FALSE,     # ← 추가
    normality = setNames(logical(0), character(0))   # ← 추가
    
    
  )
  

  
  # 로딩 -------------------------------------------------------
  # 체크 on/off — rv$selected_vars만 갱신 (DOM 상태를 다시 읽지 않음)
  observeEvent(input$var_toggle, {
    mv <- input$var_toggle
    req(is.list(mv), !is.null(mv$var), !is.null(mv$on))
    v  <- as.character(mv$var)
    on <- isTRUE(mv$on)
    cur <- rv$selected_vars %||% character(0)
    rv$selected_vars <- if (on) union(cur, v) else setdiff(cur, v)
    # 순서 목록에 없으면 뒤에 추가
    if (!(v %in% rv$var_order)) rv$var_order <- c(rv$var_order, v)
  })
  

  observeEvent(input$example_data, ignoreInit = FALSE, {   # ← FALSE 로 바꿈
    rv$loading <- TRUE
    on.exit({ rv$loading <- FALSE }, add = TRUE)
    
    if (identical(input$example_data, "gtsummary::trial")) {
      data("trial", package = "gtsummary", envir = environment())
      rv$df <- get("trial", envir = environment())
      rv$group_var <- NULL
      rv$selected_vars <- character(0)                     # trial은 기본 선택 없음
      rv$var_order <- setdiff(names(rv$df), rv$group_var)
      rv$normality <- compute_normality(rv$df, rv$group_var)
      
    } else if (identical(input$example_data, "mtcars")) {
      rv$df <- as_tibble(mtcars, .name_repair = "check_unique") |>
        mutate(am = factor(am, labels = c("Auto", "Manual")))
      rv$group_var <- "am"
      # 기본 4개 고정 (원하는 4개로 교체 가능)
      base4 <- c("mpg","hp","wt","qsec")
      rv$selected_vars <- intersect(base4, setdiff(names(rv$df), rv$group_var))
      rv$var_order <- setdiff(names(rv$df), rv$group_var)
      rv$normality <- compute_normality(rv$df, rv$group_var)
      
    }
  })
  
  
  observeEvent(input$file_csv, ignoreInit = TRUE, {
    rv$loading <- TRUE
    on.exit({ rv$loading <- FALSE }, add = TRUE)
    
    req(input$file_csv$datapath)
    df <- tryCatch(read_csv(input$file_csv$datapath, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    if (!is.null(df)) {
      rv$df <- as_tibble(df, .name_repair = "check_unique")
      rv$group_var <- NULL
      rv$selected_vars <- character(0)
      rv$var_order <- setdiff(names(rv$df), rv$group_var)  # ← 이 줄이 “바로 여기”
      rv$normality <- compute_normality(rv$df, rv$group_var)
      
      
    }
  })
  

  
  # JSON 업로드 로더 (UI의 fileInput("file_json", ...)와 한 쌍)
  observeEvent(input$file_json, ignoreInit = TRUE, {
    # ↓ 성능 패치용 로딩 플래그를 이미 쓰고 있다면 주석 해제
     rv$loading <- TRUE
     on.exit({ rv$loading <- FALSE }, add = TRUE)
    
    req(input$file_json$datapath)
    
    st <- tryCatch(state_from_json(input$file_json$datapath), error = function(e) NULL)
    if (is.null(st) || is.null(st$df)) return(NULL)
    
    # 데이터프레임
    rv$df <- as_tibble(st$df, .name_repair = "check_unique")
    
    # 그룹변수 (없거나 컬럼에 없으면 NULL)
    rv$group_var <- if (!is.null(st$group_var) && st$group_var %in% names(rv$df)) st$group_var else NULL
    
    # 선택변수 (실제 존재하는 컬럼만) + 그룹변수는 제외
    rv$selected_vars <- intersect(st$selected_vars %||% character(0), names(rv$df))
    if (!is.null(rv$group_var)) {
      rv$selected_vars <- setdiff(rv$selected_vars, rv$group_var)
    }
    
    # 표시 순서(그룹변수 제외 전체)
    rv$var_order <- setdiff(names(rv$df), rv$group_var)
    rv$normality <- compute_normality(rv$df, rv$group_var)
    
  })
  # 그룹변수 없으면 Overall 비활성화(+강제 OFF), 있으면 활성화
  observe({
    if (is.null(rv$group_var)) {
      shinyjs::disable("show_overall")
      updateCheckboxInput(session, "show_overall", value = FALSE)
    } else {
      shinyjs::enable("show_overall")
    }
  })
  
  # 그룹 변수 UI ----------------------------------------------
  


  output$ui_group_var <- renderUI({
    if (is.null(rv$df)) {
      tagList(helpText("데이터를 먼저 불러오세요."))
    } else {
      cand <- get_group_candidates(rv$df)
      
      # 빈 문자열 대신 센티널 사용 (.__none__.)
      choices_named <- c("— 선택 안함 —" = ".__none__.", setNames(cand, cand))
      
      selectizeInput(
        inputId   = "group_var",
        label     = "그룹 변수 선택 (선택 안 하면 전체 요약)",
        choices   = choices_named,
        selected  = if (is.null(rv$group_var)) ".__none__." else rv$group_var,
        options   = list(allowEmptyOption = TRUE)  # selectizeInput에서는 유효
      )
    }
  })
  
  
  observeEvent(input$group_var, {
    rv$group_var <- if (is.null(input$group_var) || input$group_var == "" || identical(input$group_var, ".__none__.")) {
      NULL
    } else {
      input$group_var
    }
    
    # 그룹 변수가 지정되면 var_order에서 제외
    if (!is.null(rv$group_var)) {
      rv$var_order <- setdiff(rv$var_order, rv$group_var)
    }
    
    # 정규성 결과 업데이트
    rv$normality <- compute_normality(rv$df, rv$group_var)
  })
  
  
    

  observeEvent(input$move_var, ignoreInit = TRUE, {
    mv <- input$move_var
    req(is.list(mv), !is.null(mv$var), !is.null(mv$dir))
    v   <- as.character(mv$var)
    dir <- as.character(mv$dir)
    
    cur <- unique(rv$var_order)
    if (!length(cur)) cur <- setdiff(names(rv$df), rv$group_var)
    
    pos <- match(v, cur); if (is.na(pos)) return()
    if (identical(dir, "up") && pos > 1) {
      cur[c(pos-1, pos)] <- cur[c(pos, pos-1)]
    } else if (identical(dir, "down") && pos < length(cur)) {
      cur[c(pos+1, pos)] <- cur[c(pos, pos+1)]
    }
    rv$var_order <- cur
  })
  
    
  
  # 현재 변수 목록 (그룹변수 제외, 순서는 gtsummary 기준의 label 행 순서로 맞추기)

  vars_all <- reactive({
    req(rv$df)
    base_tbl <- build_tbl(rv$df, rv$group_var, include_vars = NULL)
    if (is.null(base_tbl)) return(character(0))
    tb <- base_tbl$table_body
    gt_vars <- tb$variable[tb$row_type == "label"] %||% setdiff(names(rv$df), rv$group_var)
    # rv$var_order가 있으면 그것을 기준으로 표시 순서를 정렬
    if (length(rv$var_order)) {
      inter <- intersect(rv$var_order, gt_vars)
      c(inter, setdiff(gt_vars, inter))
    } else {
      unique(gt_vars)
    }
  })
  
  
  # 인라인 체크박스가 삽입된 gtsummary → gt

  # ---- (기존 server 내부) 이 부분만 교체 ----


  # --- 이 블록으로 output$gt_all 전체 교체 ---
  output$gt_all <- gt::render_gt({
    req(rv$df)
    
    # 1) gtsummary 생성
    base_tbl <- build_tbl(rv$df, rv$group_var, include_vars = NULL,
                          show_overall = isTRUE(input$show_overall),
                          show_n       = isTRUE(input$show_n))
    shiny::validate(shiny::need(!is.null(base_tbl), "요약을 생성할 수 없습니다."))
    #if (is.null(base_tbl)) {
  #shiny::validate("요약을 생성할 수 없습니다.")
   # }
    
    tb <- base_tbl$table_body
    tb$..orig <- seq_len(nrow(tb))  # 원래 행 순서
    
    # 2) 현재 라벨행의 변수 목록
    label_rows <- which(tb$row_type == "label")
    gt_vars <- tb$variable[label_rows]
    
    # 3) 사용자 순서 우선(없으면 기본 순서)
    var_levels <- if (length(rv$var_order)) {
      c(intersect(rv$var_order, gt_vars), setdiff(gt_vars, rv$var_order))
    } else gt_vars
    
    # 4) "변수 블록" 단위로 재배치 (같은 variable의 모든 행을 함께 이동)
    ord_var <- match(tb$variable, var_levels)
    na_mask <- is.na(ord_var)
    ord_var[na_mask] <- length(var_levels) + tb$..orig[na_mask]
    tb <- tb[order(ord_var, tb$..orig), , drop = FALSE]
    
    # 재배치 후 라벨행/변수 순서
    label_rows_new <- which(tb$row_type == "label")
    vars_ordered   <- tb$variable[label_rows_new]
    
    # 5) controls 새 컬럼: 체크 + 예쁜 화살표 버튼
    if (!"controls" %in% names(tb)) tb$controls <- NA_character_
    
    id_for_var <- function(v) paste0("pick__", gsub("[^A-Za-z0-9_]", "_", v))
    btn_css <- "border:none;background:none;cursor:pointer;line-height:1;padding:0 6px;"
    icon_up   <- "&#9650;"  # ▲
    icon_down <- "&#9660;"  # ▼
    
    html_controls <- vapply(
      vars_ordered,
      function(var) {
        id <- id_for_var(var)
        checked_attr <- if (var %in% (rv$selected_vars %||% character(0))) "checked=\"checked\"" else ""
        sprintf(
          "<div style='white-space:nowrap;display:flex;align-items:center;gap:6px;'>
           <input type='checkbox' id='%s' %s
                  onchange=\"event.stopPropagation();
                             Shiny.setInputValue('var_toggle',{var:'%s',on:this.checked,nonce:Date.now()},{priority:'event'})\">
           <button type='button' title='위로' aria-label='위로'
                   onclick=\"event.stopPropagation();
                             Shiny.setInputValue('move_var',{var:'%s',dir:'up',nonce:Date.now()},{priority:'event'})\"
                   style='%sfont-size:13px;'>%s</button>
           <button type='button' title='아래로' aria-label='아래로'
                   onclick=\"event.stopPropagation();
                             Shiny.setInputValue('move_var',{var:'%s',dir:'down',nonce:Date.now()},{priority:'event'})\"
                   style='%sfont-size:13px;'>%s</button>
         </div>",
          id, checked_attr, htmltools::htmlEscape(var),
          htmltools::htmlEscape(var), btn_css, icon_up,
          htmltools::htmlEscape(var), btn_css, icon_down
        )
      },
      character(1)
    )
    
    tb$controls <- NA_character_
    tb$controls[label_rows_new] <- html_controls
    
    # 보조 컬럼 제거
    tb$..orig <- NULL
    
    # 6) gtsummary에 반영 + 헤더 등록 + HTML 렌더 + 맨 왼쪽 이동
    base_tbl2 <- base_tbl |>
      gtsummary::modify_table_body(~tb) |>
      gtsummary::modify_header(update = list(controls ~ "")) |>
      gtsummary::modify_table_styling(
        columns = "controls",
        hide = FALSE,
        text_interpret = "html",
        align = "left"
      )
    
    gtsummary::as_gt(base_tbl2) |>
      gt::text_transform(
        locations = gt::cells_body(columns = "controls", rows = label_rows_new),
        fn = function(x) lapply(html_controls, gt::html)
      ) |>
      gt::cols_move_to_start(columns = "controls") |>
      gt::cols_width(controls ~ gt::px(110))
  })
  
  
  # (선택 상태 읽기 로직은 그대로)


  
  # Final Table ------------------------------------------------
  

  # Final 테이블을 탭 활성화시에만 계산

  
  

  # Final 테이블은 탭 열릴 때만 계산 + 서버 상태만 사용
  final_tbl <- eventReactive({
    list(rv$selected_vars, rv$var_order, rv$group_var, input$tabs, rv$loading)
  }, {
    req(identical(input$tabs, "Final Table"))
    req(!isTRUE(rv$loading))           # ← 로딩 중이면 계산 안 함
    req(rv$df)
    
    sel <- rv$selected_vars %||% character(0)
    req(length(sel) > 0)               # ← 선택 없으면 바로 중단
    
    if (length(rv$var_order)) sel <- intersect(rv$var_order, sel)
    build_tbl(rv$df, rv$group_var, include_vars = sel)
  }, ignoreInit = TRUE)
  
  


  # Final 탭: 열렸을 때만 즉시 계산 (eventReactive 없이)
  # Final 탭: 열렸을 때만 그리고, 동일 상태면 캐시 재사용
  output$gt_final <- gt::render_gt({
    req(identical(input$tabs, "Final Table"))
    
    req(rv$df)
    sel <- rv$selected_vars %||% character(0)
    shiny::validate(shiny::need(length(sel) > 0, "선택된 변수가 없습니다. Variable Selection 탭에서 체크하세요."))
    
    # 선택 순서 반영
    if (length(rv$var_order)) sel <- intersect(rv$var_order, sel)
    
    ft <- build_tbl(
      rv$df, rv$group_var, include_vars = sel,
      show_overall = isTRUE(input$show_overall),
      show_n       = isTRUE(input$show_n)
    )
    
    gtsummary::as_gt(ft) |> gt::tab_options(table.width = gt::px(800))
  }) |>
    bindCache(
      # 캐시 키들: 그룹/선택/순서/데이터 스펙 + 옵션
      rv$group_var,
      rv$selected_vars,
      rv$var_order,
      nrow(rv$df),
      names(rv$df),
      isTRUE(input$show_overall),
      isTRUE(input$show_n)
    )
  
  # 숨겨졌을 땐 계산 중단
  outputOptions(output, "gt_final", suspendWhenHidden = TRUE)
  

  
  
  # 상태 저장 --------------------------------------------------
  
  output$download_state <- downloadHandler(
    filename = function() paste0("variable_picker_state_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) {
      req(rv$df)
      txt <- state_to_json(rv$df, rv$group_var, rv$selected_vars)
      writeLines(txt, con = file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
