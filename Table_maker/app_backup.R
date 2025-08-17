# app.R
# ------------------------------------------------------------
# Variable Picker for Publication Tables (inline checkboxes in table)
# - Load CSV or JSON (or example data)
# - Pick a grouping variable
# - Variable Selection tab: gtsummary 표의 변수명 옆에 체크박스 삽입
# - Final Table tab: 체크된 변수만 깔끔 요약
# ------------------------------------------------------------

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
build_tbl <- function(df, group_var, include_vars = NULL) {
  req(df)
  if (!is.null(group_var) && group_var %in% names(df)) {
    df <- coerce_group_factor(df, group_var)
  } else {
    group_var <- NULL
  }
  
  data_for_tbl <- df
  if (!is.null(include_vars)) {
    keep <- unique(c(group_var, include_vars))
    keep <- keep[keep %in% names(df)]
    if (length(keep) == 0L) return(NULL)
    data_for_tbl <- df[, keep, drop = FALSE]
  }
  
  tryCatch({
    if (is.null(group_var)) {
      tbl_summary(data_for_tbl, missing = "ifany") |>
        modify_header(label ~ "**Variable**") |>
        bold_labels()
    } else {
      tbl_summary(data_for_tbl, by = !!sym(group_var), missing = "ifany") |>
        add_p() |>
        modify_header(label ~ "**Variable**") |>
        bold_labels()
    }
  }, error = function(e) NULL)
}

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

# ----- UI -----

ui <- fluidPage(
  titlePanel("Variable Picker for Publication Tables"),
  sidebarLayout(
    sidebarPanel(
      h4("1) 데이터 불러오기"),
      fileInput("file_csv", "CSV 업로드", accept = c(".csv")),
      fileInput("file_json", "JSON 업로드(상태+데이터)", accept = c(".json")),
      pickerInput("example_data", "또는 예제 데이터", choices = example_datasets, multiple = FALSE, options = list(size = 6)),
      br(),
      h4("2) 그룹 변수"),
      uiOutput("ui_group_var"),
      helpText("문자/범주형/저카디널리티 숫자(≤10레벨) 열을 제안합니다."),
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
          gt_output("gt_all")  # 테이블 내부에 체크박스 삽입
        ),
        tabPanel(
          title = "Final Table",
          br(),
          helpText("체크된 변수들만 그룹 비교하여 출력합니다."),
          gt_output("gt_final")
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
    var_order = character(0)   # << 추가: 변수 표시/최종표 출력 순서
  )
  

  
  # 로딩 -------------------------------------------------------
  
  observeEvent(input$example_data, ignoreInit = TRUE, {
    if (identical(input$example_data, "gtsummary::trial")) {
      data("trial", package = "gtsummary", envir = environment())
      rv$df <- get("trial", envir = environment())
      rv$group_var <- NULL
      rv$selected_vars <- character(0)
      rv$var_order <- setdiff(names(rv$df), rv$group_var)  # ← 이 줄이 “바로 여기”
      
    } else if (identical(input$example_data, "mtcars")) {
      rv$df <- as_tibble(mtcars, .name_repair = "check_unique") |>
        mutate(am = factor(am, labels = c("Auto", "Manual")))
      rv$group_var <- "am"
      rv$selected_vars <- setdiff(names(rv$df), rv$group_var)[1:5] %||% character(0)
      rv$var_order <- setdiff(names(rv$df), rv$group_var)  # ← 이 줄이 “바로 여기”
      
    }
  })
  
  observeEvent(input$file_csv, ignoreInit = TRUE, {
    req(input$file_csv$datapath)
    df <- tryCatch(read_csv(input$file_csv$datapath, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    if (!is.null(df)) {
      rv$df <- as_tibble(df, .name_repair = "check_unique")
      rv$group_var <- NULL
      rv$selected_vars <- character(0)
      rv$var_order <- setdiff(names(rv$df), rv$group_var)  # ← 이 줄이 “바로 여기”
      
    }
  })
  

  # 그룹 변수 UI ----------------------------------------------
  
  output$ui_group_var <- renderUI({
    if (is.null(rv$df)) {
      tagList(helpText("데이터를 먼저 불러오세요."))
    } else {
      cand <- get_group_candidates(rv$df)
      selectInput(
        "group_var",
        label = "그룹 변수 선택 (선택 안 하면 전체 요약)",
        choices = c("— 선택 안함 —" = "", cand),
        selected = rv$group_var %||% ""
      )
    }
  })
  
  observeEvent(input$group_var, { rv$group_var <- if (is.null(input$group_var) || input$group_var == "") NULL else input$group_var })
  
  observeEvent(input$move_var, {
    mv <- input$move_var
    req(is.list(mv), !is.null(mv$var), !is.null(mv$dir))
    v <- as.character(mv$var)
    dir <- as.character(mv$dir)
    
    cur <- rv$var_order
    if (!length(cur)) cur <- setdiff(names(rv$df), rv$group_var)
    
    pos <- match(v, cur)
    if (is.na(pos)) return()
    
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

  output$gt_all <- gt::render_gt({
    req(rv$df)
    
    base_tbl <- build_tbl(rv$df, rv$group_var, include_vars = NULL)
    validate(need(!is.null(base_tbl), "요약을 생성할 수 없습니다."))
    
    tb <- base_tbl$table_body
    label_rows <- which(tb$row_type == "label")
    vars <- vars_all()
    
    # 표시 순서로 table_body 재정렬
    ord_idx <- match(vars, tb$variable[label_rows])
    ord_idx <- ord_idx[!is.na(ord_idx)]
    label_rows_sorted <- label_rows[ord_idx]
    
    # 체크박스 + 이동 버튼 HTML
    id_for_var <- function(v) paste0("pick__", gsub("[^A-Za-z0-9_]", "_", v))
    is_checked <- (vars %in% (rv$selected_vars %||% character(0)))
    
    html_labels <- mapply(
      function(lbl, var, checked) {
        id <- id_for_var(var)
        flag <- if (isTRUE(checked)) "checked" else ""
        # 버튼은 JS로 Shiny input에 신호 보냄 (nonce로 중복 클릭 인식)
        lbl_html <- sprintf("
        <span style='white-space:nowrap;'>
          <input type='checkbox' id='%s' %s style='margin-right:6px;'>
          <button type='button' onclick=\"Shiny.setInputValue('move_var', {var: '%s', dir: 'up',   nonce: Date.now()}, {priority: 'event'})\" style='margin-right:4px;'>▲</button>
          <button type='button' onclick=\"Shiny.setInputValue('move_var', {var: '%s', dir: 'down', nonce: Date.now()}, {priority: 'event'})\" style='margin-right:8px;'>▼</button>
          %s
        </span>", id, flag, htmltools::htmlEscape(var), htmltools::htmlEscape(var), lbl)
        gt::html(lbl_html)
      },
      lbl = tb$label[label_rows_sorted],
      var = tb$variable[label_rows_sorted],
      checked = is_checked[match(tb$variable[label_rows_sorted], vars)],
      SIMPLIFY = FALSE
    )
    
    # 라벨 셀만 HTML로 치환 (정렬 반영)
    gt_tbl <- gtsummary::as_gt(base_tbl) |>
      gt::text_transform(
        locations = gt::cells_body(columns = "label", rows = label_rows_sorted),
        fn = function(x) html_labels
      )
    
    gt_tbl
  })
  
  
  # (선택 상태 읽기 로직은 그대로)
  selected_from_checks <- reactive({
    va <- vars_all()
    if (length(va) == 0) return(character(0))
    ids <- sapply(va, function(v) paste0("pick__", gsub("[^A-Za-z0-9_]", "_", v)))
    vals <- vapply(ids, function(id) isTRUE(input[[id]]), logical(1))
    sel <- va[vals]
    if (length(sel) == 0 && length(rv$selected_vars) > 0) rv$selected_vars else sel
  })
  
  
  # 체크박스 상태 읽어와 선택 변수로 반영
  selected_from_checks <- reactive({
    va <- vars_all()
    if (length(va) == 0) return(character(0))
    ids <- id_for_var(va)
    vals <- vapply(ids, function(id) isTRUE(input[[id]]), logical(1))
    sel <- va[vals]
    # 아무 입력이 아직 생성 안 된 초기상태면 기존 rv$selected_vars 유지
    if (length(sel) == 0 && length(rv$selected_vars) > 0) rv$selected_vars else sel
  })
  
  observe({
    # 탭에 관계없이 체크 변화가 들어오면 갱신
    new_sel <- selected_from_checks()
    if (length(new_sel)) {
      unknown <- setdiff(new_sel, rv$var_order)
      if (length(unknown)) rv$var_order <- c(rv$var_order, unknown)
    }
    rv$selected_vars <- new_sel %||% character(0)
  })
  
  # Final Table ------------------------------------------------
  

  # Final 테이블을 탭 활성화시에만 계산
  final_tbl <- eventReactive({
    # 트리거: (a) 체크 상태 변경, (b) 탭 변경
    list(selected_from_checks(), input$tabs)
  }, {
    req(rv$df)
    # 탭이 실제로 Final Table일 때만 계산
    req(identical(input$tabs, "Final Table"))
    
    sel <- selected_from_checks()
    if (length(rv$var_order)) sel <- intersect(rv$var_order, sel)
    
    if (length(sel) == 0) return(NULL)
    
    
    
    build_tbl(rv$df, rv$group_var, include_vars = sel)
  }, ignoreInit = TRUE)
  
  output$gt_final <- gt::render_gt({
    req(identical(input$tabs, "Final Table"))   # 안전장치
    ft <- final_tbl()
    if (is.null(ft)) {
      return(gt::gt(data.frame(메시지 = "선택된 변수가 없습니다. Variable Selection 탭에서 체크하세요.")))
    }
    gtsummary::as_gt(ft) |> gt::tab_options(table.width = gt::px(800))
  })
  
  
  
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
