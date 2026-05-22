# PS_AI/optimize.R
# Claude API 기반 성향점수 최적 설정 탐색기
# 실행: source("PS_AI/optimize.R")
# 환경변수 ANTHROPIC_API_KEY 필요

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(MatchIt)
  library(WeightIt)
  library(cobalt)
  library(survey)
  library(smd)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(rmarkdown)
})

# ── 0. API key 조기 검증 ──────────────────────────────────────────────
api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(api_key) == 0)
  stop("ANTHROPIC_API_KEY 환경변수를 설정하세요.\n",
       "  usethis::edit_r_environ() 실행 후 ANTHROPIC_API_KEY=sk-ant-... 추가, R 재시작")

# ── 유틸 ──────────────────────────────────────────────────────────────
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

apply_trim <- function(w, do_trim, pct = 0.99) {
  if (!isTRUE(do_trim)) return(w)
  pmin(w, quantile(w, pct, na.rm = TRUE))
}

# PS_Explorer compute_balance_stats와 동일한 SMD 계산 (matching/IPTW 공통)
calc_survey_smd <- function(df_data, grp_var, cov_names, weights) {
  t_vals <- df_data[[grp_var]]
  vapply(cov_names, function(v) {
    x        <- df_data[[v]]
    complete <- !is.na(x) & !is.na(t_vals)
    xc <- x[complete]; gc <- t_vals[complete]; wc <- weights[complete]
    if (length(unique(gc)) < 2) return(NA_real_)
    tryCatch(
      abs(smd::smd(x = xc, g = factor(gc), w = wc, na.rm = TRUE)$estimate),
      error = function(e) NA_real_
    )
  }, FUN.VALUE = numeric(1))
}

parse_cfg_json <- function(text) {
  text <- gsub("```json\\s*|```\\s*", "", text)
  text <- trimws(text)

  tryCatch(
    fromJSON(text, simplifyVector = TRUE),
    error = function(e) {
      # 중괄호 매칭으로 JSON 추출
      chars <- strsplit(text, "")[[1]]
      start <- which(chars == "{")[1]
      if (is.na(start)) stop("JSON 객체 없음")
      depth <- 0L
      end   <- NA_integer_
      for (k in start:length(chars)) {
        if (chars[k] == "{") depth <- depth + 1L
        else if (chars[k] == "}") {
          depth <- depth - 1L
          if (depth == 0L) { end <- k; break }
        }
      }
      if (is.na(end)) stop("JSON 닫힘 괄호 없음")
      fromJSON(paste(chars[start:end], collapse = ""), simplifyVector = TRUE)
    }
  )
}

# ── 1. 설정 로드 ───────────────────────────────────────────────────────
cat("=== PS_AI 최적화 시작 ===\n\n")
settings   <- fromJSON("PS_AI/settings.json", simplifyVector = TRUE)
mode       <- tolower(settings$mode)
treat      <- settings$treat
estimand   <- toupper(settings$estimand)
smd_cutoff <- settings$smd_cutoff
opt        <- settings$option

df <- read_csv("PS_AI/pre_match.csv", show_col_types = FALSE)

# treat 변수를 0/1 정수로 정규화
if (is.logical(df[[treat]])) df[[treat]] <- as.integer(df[[treat]])

candidates <- settings$candidates
if (length(candidates) == 0) candidates <- setdiff(names(df), treat)

n_treat  <- sum(df[[treat]] == 1, na.rm = TRUE)
n_ctrl   <- sum(df[[treat]] == 0, na.rm = TRUE)
min_rate      <- if (mode == "matching") as.numeric(opt$matching_rate %||% 0.9) else 0
fixed_method  <- { m <- tolower(opt$method %||% "auto"); if (m == "auto" || nchar(m) == 0) NULL else m }
fixed_ratio   <- { r <- opt$ratio %||% -1; if (identical(r, -1)) NULL else r }

# NA 포함 변수 파악 (candidates 기준, 스크립트 시작 시 1회만)
na_vars <- names(which(colSums(is.na(df[candidates])) > 0))

method_disp <- if (mode == "matching") {
  sprintf(" (%s)", if (!is.null(fixed_method)) fixed_method else "auto")
} else ""
cat(sprintf("모드: %s%s | 처치변수: %s | Estimand: %s\n", mode, method_disp, treat, estimand))
cat(sprintf("데이터: %d행 | 처치군: %d | 대조군: %d\n", nrow(df), n_treat, n_ctrl))
if (mode == "matching") {
  cat(sprintf("공변량 후보: %d개 | SMD 임계값: %.2f | 최소 매칭율: %.0f%%\n",
              length(candidates), smd_cutoff, min_rate * 100))
} else {
  cat(sprintf("공변량 후보: %d개 | SMD 임계값: %.2f\n",
              length(candidates), smd_cutoff))
}
if (length(na_vars) > 0)
  cat(sprintf("결측치 포함 변수 (%d개, 선택 시 해당 환자 제외됨): %s\n",
              length(na_vars), paste(na_vars, collapse = ", ")))
cat("\n")

# ── 2. 분석 실행 함수 ─────────────────────────────────────────────────

run_matching <- function(cfg) {
  covs <- cfg$covariates
  invalid <- covs[!covs %in% names(df)]
  if (length(invalid) > 0)
    cat(sprintf("  [경고] 없는 공변량 제외: %s\n", paste(invalid, collapse = ", ")))
  covs <- covs[covs %in% names(df)]
  if (length(covs) == 0) return(NULL)

  # 전체 원본 df에서 선택 공변량 기준 complete.cases (df 불변)
  cols  <- c(treat, covs)
  df_cc <- df[complete.cases(df[, cols, drop = FALSE]), ]
  n_drop <- nrow(df) - nrow(df_cc)
  if (n_drop > 0) cat(sprintf("  (결측 %d행 제거 → %d행)\n", n_drop, nrow(df_cc)))

  n_treat_cc <- sum(df_cc[[treat]] == 1, na.rm = TRUE)
  n_ctrl_cc  <- sum(df_cc[[treat]] == 0, na.rm = TRUE)

  f <- as.formula(paste(treat, "~", paste(covs, collapse = " + ")))

  ratio_raw <- if (!is.null(fixed_ratio)) fixed_ratio else cfg$ratio %||% 1
  ratio_val <- if (identical(ratio_raw, -1) || toupper(as.character(ratio_raw)) == "N") {
    floor(n_ctrl_cc / n_treat_cc)  # CC 기준 대조군 / CC 기준 처치군
  } else {
    max(1L, as.integer(ratio_raw))
  }

  caliper_val <- {
    cv <- as.numeric(cfg$caliper %||% 0)
    if (is.na(cv) || cv <= 0) NULL else cv
  }

  method_val <- fixed_method %||% tolower(cfg$method %||% "nearest")
  dist_raw   <- tolower(cfg$distance %||% "logit")
  glm_link   <- switch(dist_raw, "probit" = "probit", "linear.logit" = "linear.logit", NULL)
  dist_val   <- if (!is.null(glm_link)) "glm" else dist_raw

  res <- tryCatch(
    MatchIt::matchit(f, data = df_cc,
                     method   = method_val,
                     distance = dist_val,
                     link     = glm_link,
                     ratio    = if (method_val == "full") NULL else ratio_val,
                     caliper  = caliper_val,
                     estimand = estimand),
    error = function(e) { cat("  [MatchIt 오류]", conditionMessage(e), "\n"); NULL }
  )
  res
}

last_iptw_fail <- ""

run_iptw <- function(cfg) {
  covs <- cfg$covariates
  invalid <- covs[!covs %in% names(df)]
  if (length(invalid) > 0)
    cat(sprintf("  [경고] 없는 공변량 제외: %s\n", paste(invalid, collapse = ", ")))
  covs <- covs[covs %in% names(df)]
  if (length(covs) == 0) return(NULL)

  # 전체 원본 df에서 선택 공변량 기준 complete.cases (df 불변)
  cols  <- c(treat, covs)
  df_cc <- df[complete.cases(df[, cols, drop = FALSE]), ]
  n_drop <- nrow(df) - nrow(df_cc)
  if (n_drop > 0) cat(sprintf("  (결측 %d행 제거 → %d행)\n", n_drop, nrow(df_cc)))

  f <- as.formula(paste(treat, "~", paste(covs, collapse = " + ")))
  m <- tolower(cfg$method %||% "ps")
  if (m == "auto") m <- "ps"

  fail_msgs <- character(0)

  wt <- withCallingHandlers(
    tryCatch(
      WeightIt::weightit(f, data = df_cc,
                         method    = m,
                         estimand  = estimand,
                         stabilize = isTRUE(cfg$stabilize) && m == "ps"),
      error = function(e) {
        cat("  [WeightIt 오류]", conditionMessage(e), "\n")
        fail_msgs <<- c(fail_msgs, conditionMessage(e))
        NULL
      }
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("degenerate|All weights are NA|converge|maxit", msg, ignore.case = TRUE)) {
        cat("  [WeightIt 경고]", msg, "\n")
        fail_msgs <<- c(fail_msgs, msg)
      }
      invokeRestart("muffleWarning")
    }
  )

  # 퇴화 솔루션 명시적 감지
  if (!is.null(wt)) {
    t_idx <- df_cc[[treat]] == 1
    if (all(wt$weights[t_idx] == 0 | is.na(wt$weights[t_idx]))) {
      fail_msgs <<- c(fail_msgs, "All weights are zero/NA in treatment group (degenerate solution)")
      wt <- NULL
    }
  }

  last_iptw_fail <<- paste(fail_msgs, collapse = "; ")

  if (!is.null(wt) && isTRUE(cfg$trim)) {
    wt$weights <- apply_trim(wt$weights, TRUE)
  }
  wt
}

# ── 3. Balance 계산 ───────────────────────────────────────────────────

compute_balance <- function(result, cfg) {
  if (is.null(result)) return(NULL)

  tryCatch({
    cols  <- c(treat, cfg$covariates)
    df_cc <- df[complete.cases(df[, cols, drop = FALSE]), ]

    if (inherits(result, "matchit")) {
      # ── Matching: survey SMD on matched data (PS_Explorer Balance Table과 동일)
      md       <- MatchIt::match.data(result)
      all_covs <- c(cfg$covariates, setdiff(candidates, cfg$covariates))
      smd_vec  <- calc_survey_smd(md, treat, all_covs, md$weights)
      smd_vec  <- smd_vec[!is.na(smd_vec)]

      matched_n      <- sum(md[[treat]] == 1, na.rm = TRUE)
      matched_n_ctrl <- sum(md[[treat]] == 0, na.rm = TRUE)
      rate           <- matched_n / n_treat
      effective_n    <- matched_n
      ess_t          <- NA_real_; ess_c <- NA_real_
      wsum_treat     <- NA_real_; wsum_ctrl <- NA_real_
    } else {
      # ── IPTW: survey 기반 SMD (PS_Explorer Balance Table과 동일)
      all_covs <- c(cfg$covariates, setdiff(candidates, cfg$covariates))
      w <- result$weights
      t <- df_cc[[treat]]
      smd_vec <- calc_survey_smd(df_cc, treat, all_covs, w)
      smd_vec <- smd_vec[!is.na(smd_vec)]

      ess_t      <- sum(w[t == 1])^2 / sum(w[t == 1]^2)
      ess_c      <- sum(w[t == 0])^2 / sum(w[t == 0]^2)
      wsum_treat <- round(sum(w[t == 1]))
      wsum_ctrl  <- round(sum(w[t == 0]))
      effective_n    <- round(min(ess_t, ess_c))
      matched_n      <- nrow(df)
      matched_n_ctrl <- NA_integer_
      rate           <- 1.0
      min_rate       <<- 0.0
    }

    list(
      smd            = smd_vec,
      matched_n      = matched_n,
      matched_n_ctrl = matched_n_ctrl,
      effective_n    = effective_n,
      ess_treat      = round(ess_t),
      ess_ctrl       = round(ess_c),
      wsum_treat     = if (inherits(result, "matchit")) NA_real_ else wsum_treat,
      wsum_ctrl      = if (inherits(result, "matchit")) NA_real_ else wsum_ctrl,
      matching_rate  = rate,
      balanced       = all(smd_vec < smd_cutoff)
    )
  }, error = function(e) {
    cat("  [Balance 오류]", conditionMessage(e), "\n"); NULL
  })
}

# ── 4. 메시지 포맷 ────────────────────────────────────────────────────

format_result_msg <- function(iter, cfg, bal) {
  cfg_str <- toJSON(cfg, auto_unbox = TRUE)

  if (is.null(bal)) {
    fail_hint  <- if (nchar(last_iptw_fail) > 0)
      sprintf("Failure reason: %s\n", last_iptw_fail) else ""
    ebal_hint  <- if (grepl("degenerate|zero|converge", last_iptw_fail, ignore.case = TRUE))
      "ebal failed — immediately switch to method 'ps' or 'cbps', or reduce covariates.\n" else ""
    return(sprintf(
      "Iteration %d: FAILED.\n%s%sConfig: %s\n\nPlease try a different configuration.",
      iter, fail_hint, ebal_hint, cfg_str
    ))
  }

  n_line <- if (mode == "matching") {
    sprintf("Matched N (treated): %d / %d | Rate: %.1f%%",
            bal$matched_n, n_treat, bal$matching_rate * 100)
  } else {
    sprintf("Σw treat: %d, Σw ctrl: %d (all %d obs used, ESS: %d)",
            bal$wsum_treat, bal$wsum_ctrl, bal$matched_n, bal$effective_n)
  }

  smd_lines <- paste(
    sprintf("  %-22s %.4f  %s", names(bal$smd), bal$smd,
            ifelse(bal$smd < smd_cutoff, "OK",
                   sprintf("IMBALANCED (>%.2f)", smd_cutoff))),
    collapse = "\n"
  )

  worst_var <- names(which.max(bal$smd))
  sprintf(paste0(
    "Iteration %d result:\n",
    "Config: %s\n",
    "%s\n",
    "Balanced: %s | Max |SMD|: %.4f (%s)\n",
    "SMD per covariate:\n%s\n\n",
    "Please suggest the next configuration to maximize balanced sample size."
  ),
  iter, cfg_str, n_line,
  if (bal$balanced) "YES" else "NO",
  max(bal$smd, na.rm = TRUE), worst_var,
  smd_lines)
}

# ── 5. Claude API ─────────────────────────────────────────────────────

claude_suggest <- function(messages, system_prompt) {
  req <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(list(
      model      = "claude-opus-4-7",
      max_tokens = 1024,
      system     = system_prompt,
      messages   = messages
    )) |>
    req_error(is_error = \(r) FALSE)

  max_retry <- 3
  for (attempt in seq_len(max_retry)) {
    resp   <- req_perform(req)
    status <- resp_status(resp)

    if (status == 200) {
      return(resp_body_json(resp)$content[[1]]$text)
    } else if (status %in% c(500, 503, 529) && attempt < max_retry) {
      wait <- 10 * attempt
      cat(sprintf("  [%d/%d] API %d 일시 오류, %d초 후 재시도...\n",
                  attempt, max_retry, status, wait))
      Sys.sleep(wait)
    } else {
      stop(sprintf("Claude API %d: %s", status, resp_body_string(resp)))
    }
  }
}

# ── 6. 시스템 프롬프트 ────────────────────────────────────────────────

if (mode == "matching") {
  fmt_ex <- sprintf(
    '{"covariates":[...],"caliper":0.2,"ratio":2,"method":"nearest","distance":"logit","reasoning":"..."}  (max ratio: %d)',
    floor(n_ctrl / n_treat)
  )
  rules_extra <- paste0(
    if (!is.null(fixed_method))
      sprintf("- method is FIXED to \"%s\" — do not change it\n", fixed_method)
    else
      "- method options: nearest, optimal, full\n",
    "- distance options: logit, probit, linear.logit\n",
    "- caliper 0 means no caliper\n",
    "- ratio -1 means use floor(n_ctrl/n_treat) automatically\n",
    sprintf("- matching_rate must be >= %.0f%%\n", min_rate * 100)
  )
} else {
  fmt_ex <- '{"covariates":[...],"method":"ps","stabilize":false,"trim":false,"reasoning":"..."}'
  rules_extra <- paste0(
    "- method options: ps, cbps, ebal\n",
    "- ps is the most robust; start with ps, try ebal/cbps only when ps is balanced but ESS is low\n",
    "- ebal is sensitive to large covariate sets (>15 vars) and can produce degenerate solutions (all weights=0); if ebal fails, immediately switch to ps or cbps\n"
  )
}

na_note <- if (length(na_vars) > 0) {
  sprintf(
    "Missing data note: These covariates have missing values — including them reduces sample size via complete-case analysis: %s\n",
    paste(na_vars, collapse = ", ")
  )
} else ""

system_prompt <- sprintf(paste0(
  "You are an expert biostatistician specializing in propensity score analysis.\n",
  "Goal: find the configuration that achieves covariate balance (ALL |SMD| < %.2f) ",
  "while MAXIMIZING the balanced sample size (matched N for matching, ESS for IPTW).\n\n",
  "Dataset: %d rows. Treatment='%s' (%d treated, %d control). Mode=%s, Estimand=%s.\n",
  "Available covariates (%d): %s\n",
  "%s\n",
  "Rules:\n",
  "- Respond ONLY with valid JSON. No text before or after the JSON object.\n",
  "- JSON format: %s\n",
  "%s",
  "- If a config fails, try substantially different parameters.\n",
  "- Start broad, then narrow down: first try all covariates, then remove imbalanced predictors.\n",
  "- After achieving balance, try to increase N by relaxing caliper or increasing ratio.\n",
  "- Covariates that are heavily imbalanced (|SMD| >> threshold) may need removal.\n",
  "- Covariates that are easy to balance can always be kept."
),
smd_cutoff,
nrow(df), treat, n_treat, n_ctrl, mode, estimand,
length(candidates), paste(candidates, collapse = ", "),
na_note, fmt_ex, rules_extra)

# ── 7. 메인 루프 ─────────────────────────────────────────────────────
max_iter         <- 40
no_improve_limit <- 5
messages         <- list()
all_results      <- list()
best_bal_n       <- 0L
no_improve_count <- 0L

# 초기 사용자 메시지 (API는 첫 메시지가 user여야 함)
initial_msg <- sprintf(
  "Please suggest the first configuration to try. Mode: %s, Estimand: %s. Data: %d treated, %d control. Available covariates: %s",
  mode, estimand, n_treat, n_ctrl, paste(candidates, collapse = ", ")
)
messages <- list(list(role = "user", content = initial_msg))

cat("--- 최적화 루프 시작 ---\n\n")

for (i in seq_len(max_iter)) {
  cat(sprintf("[반복 %02d/%d] Claude 요청 중... ", i, max_iter))

  raw_resp <- tryCatch(
    claude_suggest(messages, system_prompt),
    error = function(e) { cat("API 오류:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(raw_resp)) break

  cfg <- tryCatch(
    parse_cfg_json(raw_resp),
    error = function(e) {
      cat("JSON 파싱 실패:", conditionMessage(e), "\n→", raw_resp, "\n")
      NULL
    }
  )
  if (is.null(cfg)) {
    messages <- c(messages,
                  list(list(role = "assistant", content = raw_resp)),
                  list(list(role = "user",
                            content = sprintf("Iteration %d: Could not parse your response as JSON. Please respond with ONLY a valid JSON object.", i))))
    next
  }

  # IPTW: stabilize/trim은 settings.json 고정값으로 덮어씀 (Claude 임의변경 방지)
  if (mode == "iptw") {
    cfg$stabilize <- isTRUE(opt$stabilize)
    cfg$trim      <- isTRUE(opt$trim)
  }

  # 분석 실행
  result <- if (mode == "matching") run_matching(cfg) else run_iptw(cfg)
  bal    <- compute_balance(result, cfg)

  # 결과 출력
  if (!is.null(bal)) {
    if (mode == "matching") {
      rate_warn <- if (bal$matching_rate < min_rate)
        sprintf(" [<%.0f%%!]", min_rate * 100) else ""
      n_str <- sprintf("%d (%.0f%%) vs. %d (%.0f%%)%s",
                       bal$matched_n_ctrl, bal$matched_n_ctrl / n_ctrl * 100,
                       bal$matched_n,      bal$matching_rate  * 100,
                       rate_warn)
    } else {
      n_str <- sprintf("Σw %d (ESS %d) vs. Σw %d (ESS %d)",
                       bal$wsum_ctrl, bal$ess_ctrl,
                       bal$wsum_treat, bal$ess_treat)
    }
    cat(sprintf("%s, maxSMD=%.3f, bal=%s\n",
                n_str,
                max(bal$smd, na.rm = TRUE),
                if (bal$balanced) "YES" else "NO"))

    eff_n        <- if (mode == "matching") bal$matched_n else bal$effective_n
    rate_ok      <- mode != "matching" || bal$matching_rate >= min_rate
    if (bal$balanced && rate_ok) {
      if (eff_n > best_bal_n) {
        best_bal_n <- eff_n
        no_improve_count <- 0L
        cat(sprintf("  ★ 새 최적값: balanced N = %d ★\n", best_bal_n))
      } else {
        no_improve_count <- no_improve_count + 1L
      }
    }
  } else {
    cat("분석 실패\n")
  }

  all_results[[i]] <- list(iter = i, cfg = cfg, bal = bal, result = result)

  # 대화 이력 업데이트
  result_msg <- format_result_msg(i, cfg, bal)
  messages   <- c(messages,
                  list(list(role = "assistant", content = raw_resp)),
                  list(list(role = "user",      content = result_msg)))

  if (no_improve_count >= no_improve_limit) {
    cat(sprintf("\n[수렴] %d회 연속 N 개선 없음 → 루프 종료\n", no_improve_limit))
    break
  }
}

# ── 8. 최적 결과 선택 ─────────────────────────────────────────────────
all_results <- Filter(Negate(is.null), all_results)
balanced    <- Filter(function(x) !is.null(x$bal) && x$bal$balanced &&
                       (mode != "matching" || x$bal$matching_rate >= min_rate), all_results)

if (length(balanced) > 0) {
  eff_ns    <- sapply(balanced, function(x)
    if (mode == "matching") x$bal$matched_n else x$bal$effective_n)
  best_item <- balanced[[which.max(eff_ns)]]
  cat(sprintf("\n최적 설정 (반복 %d): balanced N = %d\n",
              best_item$iter,
              if (mode == "matching") best_item$bal$matched_n else best_item$bal$effective_n))
} else if (length(all_results) == 0) {
  stop("분석 결과가 없습니다. API 키 및 데이터를 확인하세요.")
} else {
  max_smds  <- sapply(all_results, function(x)
    if (is.null(x$bal)) Inf else max(x$bal$smd, na.rm = TRUE))
  best_item <- all_results[[which.min(max_smds)]]
  best_smd  <- round(max(best_item$bal$smd, na.rm = TRUE), 4)

  cutoff_tip <- if (best_smd > smd_cutoff) {
    sprintf("  1. smd_cutoff 완화: settings.json → %.2f 이상으로 수정\n",
            round(best_smd + 0.02, 2))
  } else {
    "  1. 공변량 조합 변경: 결측 없는 변수 위주로 candidates 직접 지정\n"
  }
  cat(sprintf(paste0(
    "\n══════════════════════════════════\n",
    "balanced 설정을 찾지 못했습니다.\n",
    "최선 결과 (반복 %d): max |SMD| = %.4f (임계값 %.2f)\n",
    "══════════════════════════════════\n",
    "대처 방법:\n",
    "%s",
    "  2. IPTW 시도: settings.json → mode: \"iptw\"\n",
    "  3. 공변량 직접 지정: settings.json → candidates에 변수 목록 입력\n",
    "참고용으로 최선 결과를 저장합니다.\n"
  ), best_item$iter, best_smd, smd_cutoff, cutoff_tip))
}

# ── 9. best_result.json 저장 ──────────────────────────────────────────
best_item$cfg$covariates <- best_item$cfg$covariates[
  order(match(best_item$cfg$covariates, candidates))
]
if (!is.null(fixed_ratio))  best_item$cfg$ratio  <- fixed_ratio
if (!is.null(fixed_method)) best_item$cfg$method <- fixed_method
best_out <- list(
  mode       = mode,
  treat      = treat,
  estimand   = estimand,
  smd_cutoff = smd_cutoff,
  best_config = best_item$cfg,
  result_summary = if (!is.null(best_item$bal)) list(
    balanced      = best_item$bal$balanced,
    matched_n     = best_item$bal$matched_n,
    effective_n   = best_item$bal$effective_n,
    matching_rate = round(best_item$bal$matching_rate, 4),
    max_smd       = round(max(best_item$bal$smd, na.rm = TRUE), 4),
    n_iterations  = length(all_results)
  ) else NULL
)
write(toJSON(best_out, auto_unbox = TRUE, pretty = TRUE), "PS_AI/best_result.json")
cat("best_result.json 저장 완료.\n")

# ── 10. 보고서 생성 ───────────────────────────────────────────────────
report_data <- list(
  all_results = all_results,
  best_item   = best_item,
  settings    = settings,
  df          = df,
  n_treat     = n_treat,
  n_ctrl      = n_ctrl,
  mode        = mode,
  treat       = treat,
  smd_cutoff  = smd_cutoff,
  min_rate    = min_rate,
  pre_smd     = vapply(candidates, function(v) {
    x <- df[[v]]; g <- df[[treat]]
    complete <- !is.na(x) & !is.na(g)
    if (sum(complete) == 0 || length(unique(g[complete])) < 2) return(NA_real_)
    tryCatch(
      abs(smd::smd(x = x[complete], g = factor(g[complete]), na.rm = FALSE)$estimate),
      error = function(e) NA_real_
    )
  }, FUN.VALUE = numeric(1))
)
saveRDS(report_data, "PS_AI/.report_data.rds")

if (file.exists("PS_AI/report_template.Rmd")) {
  cat("보고서 생성 중...\n")
  tryCatch({
    rmarkdown::render(
      input       = "PS_AI/report_template.Rmd",
      output_file = "report.html",
      output_dir  = "PS_AI",
      quiet       = TRUE
    )
    cat("report.html 생성 완료.\n")
    if (interactive()) browseURL("PS_AI/report.html")
  }, error = function(e) cat("보고서 생성 오류:", conditionMessage(e), "\n"))
} else {
  cat("[참고] report_template.Rmd가 없어 보고서를 생성하지 않았습니다.\n")
}

cat("\n=== PS_AI 최적화 완료 ===\n")
