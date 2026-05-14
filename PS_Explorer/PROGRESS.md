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

### Variable Selection DT pageLength 리셋 버그 수정
- **문제**: 변수 체크 시 `rv$sel` 변경 → `renderDT` 전체 재실행 → pageLength 25로 초기화
- **해결**: `datatable options`에 `stateSave = TRUE` 추가 (브라우저 localStorage에 상태 저장)

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
- **Matching**: MatchIt + cobalt (Love Plot, Balance Plot, PS Distribution `"distance"`)
- **IPTW**: WeightIt + cobalt + survey (Balance Table N=Σw, SMD survey::svyvar 방식)

---

## 알려진 제약사항 / 주의사항

- IPTW에서 cobalt에 weights 전달 시 `res_show$weights <- trimmed_w` 패턴만 사용 (직접 인자 전달 금지)
- cobalt PS 변수명: Matching=`"distance"`, IPTW=`"prop.score"` (혼용 금지)
- `compute_var_table` 내 `tryCatch` 블록에서 `return()` 절대 사용 금지
