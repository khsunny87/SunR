# CLAUDE.md — PS_Explorer

성향점수 분석 Shiny 앱. **매칭(MatchIt)** 과 **IPTW(WeightIt)** 두 방법 지원. 단일 파일 `app.R`.

## 실행

```r
shiny::runApp("PS_Explorer")
```

## 의존 패키지

`shiny`, `shinyWidgets`, `DT`, `MatchIt`, `cobalt`, `smd`, `dplyr`, `purrr`, `readr`, `ggplot2`, `survival`, `WeightIt`, `survey`

---

## 핵심 헬퍼 함수

| 함수 | 역할 |
|------|------|
| `fmt_stat(x, is_normal)` | Variable Selection용 — Shapiro-Wilk 기반 mean±SD 또는 median[IQR] |
| `fmt_stat_w(x, w, is_cont)` | Balance Table용 — `survey::svymean/svyvar` 기반 가중 통계 |
| `compute_var_table(df, grp_var)` | Variable Selection 테이블 생성 (SMD, p-value, 기술통계) |
| `compute_balance_stats(df, grp_var, covs, weights)` | Balance Table 통계 계산 |
| `apply_trim(w, trim, pct)` | IPTW 가중치 윈저화 트리밍 (기본 99th pct) |
| `highlight_imbal_rows(dt_widget)` | DT 4단계 행 강조 (both/outcome/smd/ok) |

---

## 반응형 구조

```
data_raw() → data_clean() → ps_result()
                 ↓
          var_tbl_data() → output$var_table (DT, stateSave=TRUE)
                 ↓
          outcome_pvals()
```

- `rv$sel`: 선택된 공변량 벡터 (DT 체크박스 `sel_changed` 이벤트로 갱신)
- `ps_result()`: `input$run_ps` 버튼으로 트리거, Matching 또는 IPTW 결과 반환

---

## 매칭 (MatchIt)

```r
MatchIt::matchit(formula, data, method, distance, ratio, caliper, estimand = input$match_estimand)
```

- `match_estimand`: ATT(기본) / ATE / ATC — IPTW의 `input$estimand`와 input ID 분리
- UI 순서: Estimand → Method → Ratio → 1:N스위치 → Caliper → Distance (IPTW와 동일하게 Estimand 최상단)
- 1:N 매칭 스위치 ON 시 `ratio = floor(n_ctrl / n_trt)` 자동 계산
- 스위치 UI: `uiOutput("ui_ratio")` + CSS `opacity:0.4; pointer-events:none` (shinyjs 미사용)
- cobalt Love/Balance Plot PS 변수명: `"distance"`

---

## IPTW (WeightIt) — MatchIt 절대 사용 금지

```r
WeightIt::weightit(formula, data, method, estimand, stabilize)
```

- `method`: `"ps"` / `"cbps"` / `"ebal"`
- `estimand`: `"ATE"` (기본) / `"ATT"` / `"ATC"`
- `stabilize`: `isTRUE(input$stabilize_weights)`
- 트리밍: `apply_trim(res$weights, isTRUE(input$trim_weights))`

### cobalt에 trimmed weights 전달

```r
res_show <- res
res_show$weights <- trimmed_w
# cobalt 함수에 res_show 전달 — weights= 인자 직접 전달 금지
```

직접 전달 시 cobalt가 다중 weight set으로 오인함.

- cobalt PS 변수명: `"prop.score"` (MatchIt `"distance"`와 다름)
- PS Distribution 플롯: cobalt `bal.plot()` 미사용, `ggplot2 + geom_density(weight=)` 커스텀 플롯

### SMD 계산 원칙

`smd::smd(w=)` 사용 금지 — `svyCreateTableOne(smd=T)`와 결과가 다름.

```r
# 연속변수 IPTW SMD
calc_grp <- function(vals, ws) {
  d <- survey::svydesign(ids = ~1, data = data.frame(x = vals), weights = ~ws)
  list(m = as.numeric(survey::svymean(~x, d)),
       v = as.numeric(survey::svyvar(~x, d)))
}
s1 <- calc_grp(xc[g1], wc[g1]); s2 <- calc_grp(xc[g2], wc[g2])
smd_val <- abs(s1$m - s2$m) / sqrt((s1$v + s2$v) / 2)
```

- Balance Table N 표시: Σw (가중치 합, svyCreateTableOne 방식)

---

## Variable Selection 테이블 주의사항

### tryCatch 내 return() 금지

`compute_var_table`의 `function(v)` 콜백 안에서 `tryCatch` 블록 내 `return()` 사용 시 외부 콜백 전체가 종료되어 `map_dfr`에 `NA_real_` 전달 → "Argument N must be a data frame" 크래시.

```r
# 금지
if (length(unique(gc)) < 2) return(NA_real_)

# 올바른 패턴
if (length(unique(gc)) < 2) {
  NA_real_
} else if (is_cont) { ... } else { ... }
```

### 행 강조

`.__flag__` 숨김 컬럼 기반 4단계: `both`(초록) > `outcome`(파란) > `smd`(노란) > `ok`(없음)

### DT pageLength 유지

`datatable options`에 `stateSave = TRUE` 필수 — 없으면 변수 체크마다 pageLength 25로 초기화됨.

---

## 개발 이력

→ [PROGRESS.md](PROGRESS.md) 참조
