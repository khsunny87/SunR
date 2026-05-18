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

## 알려진 제약사항 / 주의사항

- `ANTHROPIC_API_KEY` 환경변수 미설정 시 즉시 오류 종료
- `cobalt::bal.tab()` 결과의 `Diff.Adj` 컬럼 없을 시 해당 iteration 스킵
- IPTW 모드에서 `matching_rate` 옵션은 무시됨 (min_rate = 0으로 고정)
- `candidates` 빈 배열 = treat 제외 전체 컬럼 자동 사용 (pre_match.csv 기준 28개)
- 보고서 PS Distribution 플롯은 Matching 모드에서만 렌더됨 (IPTW는 해당 없음)
- 40회 × Claude API 호출 비용 발생 (수렴 시 조기 종료로 단축 가능)
