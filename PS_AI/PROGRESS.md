# PS_AI 개발 진행 현황

## v1.0 — 초기 구현 (2026-05-18)

### 개요

PS_Explorer에서 공변량·caliper·ratio를 수동으로 반복 조정하는 불편함을 해소하기 위해 개발.
Claude API(claude-opus-4-7)를 다중턴 대화로 활용해 최적 성향점수 분석 설정을 자동 탐색.

### 구현 내용

**파일 구조**
- `settings.json` — 사용자 설정 (mode, treat, estimand, smd_cutoff, candidates, option)
- `optimize.R` — 메인 최적화 스크립트 (단일 파일, `source()` 실행)
- `report_template.Rmd` — HTML 보고서 템플릿

**최적화 알고리즘**
- 최대 40회 반복, Claude와 다중턴 대화 방식
- 매 iteration: Claude가 설정 제안 → MatchIt/WeightIt 실행 → balance 계산 → 결과를 Claude에게 피드백
- 수렴 조건: balanced 상태에서 5회 연속 N 개선 없음
- 최종 선택: balanced 결과 중 최대 matched_n (Matching) 또는 최소 ESS (IPTW)

**지원 모드**
- Matching: MatchIt (`nearest`, `optimal`, `full`), caliper, ratio (`k`, `"N"`, `-1`), distance
- IPTW: WeightIt (`ps`, `cbps`, `ebal`), stabilize, trim

**보고서 구성**
- 요약표 (최적 설정 파라미터)
- Balance Table (|SMD| per covariate)
- Love Plot (cobalt)
- PS Distribution (Matching 모드, ggplot2 geom_density)
- 탐색 이력 차트 (반복 × N, 반복 × maxSMD)
- 최적 설정 JSON 원문

**의존 패키지 추가**
- `httr2` — Claude API 호출 (renv로 설치, renv.lock 업데이트)

---

---

## v1.1 — PS_Explorer 일치성 개선 (2026-05-19)

### 변경 내용

**SMD 계산 방식 통일 (optimize.R)**
- `calc_survey_smd()` 신규 헬퍼 함수 추가 — PS_Explorer `compute_balance_stats()`와 동일한 survey 기반 SMD
  - 연속변수: `svymean/svyvar` pooled weighted variance
  - 범주형: `smd::smd(x, g, w=)`
- `compute_balance()` matching/IPTW 양쪽에서 cobalt `Diff.Adj` → `calc_survey_smd()` 교체
- 효과: PS_AI Balance Table SMD = PS_Explorer Balance Table SMD (일치 보장)

**IPTW stabilize/trim 잠금 (optimize.R)**
- Claude가 제안한 cfg의 `stabilize`/`trim`을 `settings.json` 고정값으로 덮어씀
- 기존 문제: Claude가 `stabilize:true, trim:true`로 임의 변경 가능

**Love Plot 교체 (report_template.Rmd)**
- cobalt `love.plot()` → `calc_survey_smd()` 기반 ggplot2 커스텀 플롯
- pre_smd (unadjusted): `smd::smd()` 무가중, `report_data`에 저장
- post_smd (adjusted): `bal$smd` (survey SMD) 직접 사용
- 효과: Balance Table과 Love Plot의 SMD 값 일치 (판정 모순 해소)

**로그 출력 포맷 변경 (optimize.R)**
- Matching: `ctrl_N (%) vs. trt_N (%) [<rate%!]` 형식
- IPTW: `Σw ctrl vs. trt` 표시 (ESS → Σw, PS_Explorer Balance Table 헤더와 일치)
- 보고서 요약표도 ESS → Σw 표시로 변경

**IPTW ebal 오류 처리 개선 (optimize.R)**
- `withCallingHandlers()` + 퇴화 솔루션 명시적 감지
- `format_result_msg()` 실패 시 원인 메시지 + ebal 힌트를 Claude에게 전달

**의존 패키지 추가**
- `library(survey)`, `library(smd)`

---

## 알려진 제약사항 / 주의사항

- `ANTHROPIC_API_KEY` 환경변수 미설정 시 즉시 오류 종료
- `cobalt::bal.tab()` 결과의 `Diff.Adj` 컬럼 없을 시 해당 iteration 스킵
- IPTW 모드에서 `matching_rate` 옵션은 무시됨 (min_rate = 0으로 고정)
- `candidates` 빈 배열 = treat 제외 전체 컬럼 자동 사용 (pre_match.csv 기준 28개)
- 보고서 PS Distribution 플롯은 Matching 모드에서만 렌더됨 (IPTW는 해당 없음)
- 40회 × Claude API 호출 비용 발생 (수렴 시 조기 종료로 단축 가능)
