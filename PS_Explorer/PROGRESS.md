# PS_Explorer 개발 진행 현황

## 완료된 주요 기능 및 버그 수정

### SMD 계산 방식 교정
- **문제**: `smd::smd(w=)` 사용 시 `svyCreateTableOne(smd=T)` 결과와 미세 차이 발생 (예: age SMD 0.161 vs 0.166)
- **원인**: `smd::smd`는 단순 가중분산, `svyCreateTableOne`은 `survey::svyvar` 기반 분산 사용
- **해결**: 연속변수 IPTW SMD를 `survey::svydesign + svymean/svyvar`로 직접 계산
  - 공식: `|svymean_1 - svymean_2| / sqrt((svyvar_1 + svyvar_2) / 2)`

### IPTW PS Distribution 플롯 x축 클리핑 수정
- **문제**: cobalt `bal.plot()` Adjusted Sample 패널에서 PS 분포 양 끝단이 잘림
- **원인**: cobalt가 adjusted 패널에서 고밀도 영역 중심으로 x축 자동 스케일
- **해결**: cobalt 대신 `ggplot2 + geom_density(weight=)` 커스텀 플롯으로 교체
  - Unadjusted / Adjusted(IPTW) 두 패널 facet_wrap 구성

### IPTW UI 기본값 및 Stabilization 추가
- estimand 기본값: ATE (기존 ATT에서 변경)
- trimming 기본값: 비활성 (기존 활성에서 변경)
- `stabilize_weights` prettySwitch 추가 (기본값 비활성)
- `WeightIt::weightit(..., stabilize = isTRUE(input$stabilize_weights))` 반영
- 코드박스에 trimming/stabilize 옵션 문자열 반영

### compute_var_table return() 버그 수정
- **문제**: `redo_lung_surv.csv` 등 다수 컬럼 CSV 로드 시 "Argument 39 must be a data frame" 크래시
- **원인**: `tryCatch` 블록 안 `return(NA_real_)` 호출이 외부 `function(v)` 콜백 전체를 종료 → `map_dfr`가 tibble 대신 `NA_real_` 수신
- **해결**: `return()` 제거, `if/else` 구조로 재작성

### 1:N 매칭 스위치
- Ratio (k:1) `numericInput` 옆에 `prettySwitch("ratio_all", "1:N 매칭 (전체)")` 추가
- 스위치 ON 시: `uiOutput` + CSS `opacity:0.4; pointer-events:none`으로 ratio 입력 시각적 비활성화
- `ps_result` 내 `actual_ratio = floor(n_ctrl / n_trt)` 자동 계산

### Matching Estimand 옵션 복원 및 위치 통일
- **문제**: IPTW estimand UI를 `conditionalPanel(condition = "input.ps_method == 'iptw'")` 안에만 넣어 Matching 탭에서 Estimand 선택이 사라짐, MatchIt 호출도 `"ATT"` 하드코딩 상태
- **해결**: `prettyRadioButtons("match_estimand", ...)` 추가 (ATT 기본, IPTW `"estimand"`와 input ID 분리)
- `MatchIt::matchit(..., estimand = input$match_estimand)` 및 코드박스에도 동적 반영
- UI 위치: Matching/IPTW 모두 섹션 h5 바로 아래 Estimand 라디오버튼으로 통일

### Variable Selection DT 페이지/pageLength 초기화 버그 수정 (2차)
- **문제**: show를 50/100으로 변경하고 2페이지에서 체크박스 클릭 시 pageLength 25 / 1페이지로 초기화
- **원인**: `renderDT` 내 `sel_now <- rv$sel` 가 `rv$sel` 을 reactive 의존성으로 등록 → 체크박스 클릭마다 DT 인스턴스 소멸·재생성 (`stateSave` localStorage 복원 불안정)
- **해결**: `sel_now <- isolate(rv$sel)` 로 의존성 차단 — 체크박스 시각 상태는 클라이언트 JS에서 관리하므로 서버 재렌더 불필요

### Balance Table logical 그룹변수 에러 수정
- **문제**: 그룹 변수가 logical(TRUE/FALSE) 타입이면 Balance Table에서 "Error: [object Object]"
- **원인**: `compute_balance_stats`가 grp_var를 logical로 받으면 `grp_FALSE`/`grp_TRUE` 컬럼 생성, 외부 조립 코드는 `grp_0`/`grp_1` 접근 → NULL → DT 에러
- **해결**: `output$bal_tbl` 진입 시 `if (is.logical(df[[grp]])) df[[grp]] <- as.integer(df[[grp]])` 로 0/1 정규화

### Love Plot — cobalt → survey SMD ggplot2 교체 (2026-05-19)
- **문제**: cobalt `love.plot()` SMD(Diff.Adj)와 Balance Table SMD(survey) 값이 달라 같은 결과에서 판정이 엇갈림
- **해결**: `compute_balance_stats()` + ggplot2 커스텀 플롯으로 교체 → Balance Table과 동일한 SMD
  - pre (Unadjusted): `weights=NULL` → `smd::smd()` 무가중
  - post (Adjusted): `weights = matched_df$weights` 또는 `apply_trim(res$weights, ...)`
  - Y축 순서: unadjusted SMD 오름차순 (낮은 값 하단)

### fmt_stat_w 단일 레벨 factor 에러 수정
- **문제**: 매칭 후 한 그룹에 범주형 변수 값이 하나만 남으면 `survey::svymean` → "contrasts can be applied only to factors with 2 or more levels" 에러
- **원인**: 1레벨 factor를 `survey::svymean(~xf, d)` 에 전달 시 `model.matrix` 에서 에러
- **해결**: `fmt_stat_w` 내 `nlevels(xf) < 2` 체크 → 단순 카운트 + "(100%)" 표시로 분기

---

## 현재 앱 구조 요약

```
data_raw() → data_clean() → ps_result()
                 ↓
          var_tbl_data() → output$var_table (DT, stateSave=TRUE)
                 ↓
          outcome_pvals()
```

**분석 방법 2가지:**
- **Matching**: MatchIt + ggplot2 Love Plot + cobalt Balance Plot (`"distance"`)
- **IPTW**: WeightIt + ggplot2 Love Plot + cobalt Balance Plot (`"prop.score"`) + survey (Balance Table N=Σw, SMD)

---

## 알려진 제약사항 / 주의사항

- IPTW에서 cobalt에 weights 전달 시 `res_show$weights <- trimmed_w` 패턴만 사용 (직접 인자 전달 금지)
- cobalt PS 변수명: Matching=`"distance"`, IPTW=`"prop.score"` (혼용 금지)
- `compute_var_table` 내 `tryCatch` 블록에서 `return()` 절대 사용 금지
