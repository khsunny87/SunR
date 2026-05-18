# CLAUDE.md — PS_AI

Claude API 기반 성향점수 최적 설정 자동 탐색기. 단일 스크립트 `optimize.R`.

## 실행

```r
# .Renviron 또는 환경변수에 ANTHROPIC_API_KEY 설정 필요
source("PS_AI/optimize.R")
# → PS_AI/best_result.json 저장
# → PS_AI/report.html 자동 생성 (interactive 환경에서 브라우저 열림)
```

## 입력 파일

| 파일 | 역할 |
|------|------|
| `pre_match.csv` | 분석 대상 데이터 |
| `settings.json` | 사용자 설정 (모드, 처치변수, SMD 임계값, 탐색 옵션) |

## settings.json 스펙

```json
{
  "mode": "matching",       // "matching" or "iptw"
  "treat": "DCD",           // 처치 변수명
  "estimand": "ATT",        // "ATT" / "ATE" / "ATC"
  "smd_cutoff": 0.15,       // balance 판정 SMD 임계값
  "candidates": [],         // 공변량 후보. 빈 배열이면 treat 제외 전체 컬럼 자동 사용
  "option": {
    "ratio": -1,            // matching: k(고정) / "N"(전체) / -1(Claude가 결정)
    "matching_rate": 0.9    // matching: 처치군 최소 매칭율 (0.9 = 90%)
  }
}
```

IPTW option 파라미터:
```json
"option": {
  "method": "auto",         // "ps" / "cbps" / "ebal" / "auto"(Claude가 결정)
  "stabilize": false,
  "trim": false
}
```

## 의존 패키지

`httr2`, `jsonlite`, `MatchIt`, `WeightIt`, `cobalt`, `dplyr`, `ggplot2`, `readr`, `rmarkdown`

---

## 핵심 함수

| 함수 | 역할 |
|------|------|
| `run_matching(cfg)` | cfg JSON으로 `MatchIt::matchit()` 실행, NULL 반환 시 오류 |
| `run_iptw(cfg)` | cfg JSON으로 `WeightIt::weightit()` 실행, trim 적용 포함 |
| `compute_balance(result, cfg)` | `cobalt::bal.tab()` → SMD 벡터 + matched_n + balanced 여부 |
| `is_balanced(bal)` | 모든 SMD < smd_cutoff AND matching_rate ≥ min_rate |
| `claude_suggest(messages, system_prompt)` | httr2로 Claude API 호출, 텍스트 반환 |
| `parse_cfg_json(text)` | 마크다운 코드블록 제거 후 JSON 파싱 (중괄호 매칭 폴백 포함) |
| `format_result_msg(iter, cfg, bal)` | Claude에게 보낼 iteration 결과 텍스트 생성 |

---

## 최적화 루프 구조

```
초기 user 메시지 → Claude → cfg JSON
                              ↓
                        run_matching / run_iptw
                              ↓
                        compute_balance → bal
                              ↓
                     balanced? → best_results 업데이트
                              ↓
                     format_result_msg → messages에 추가
                              ↓
                        Claude → 다음 cfg JSON
                              ↓ (최대 40회, 수렴 시 조기 종료)
```

- 수렴 조건: balanced 상태에서 5회 연속 N 개선 없음
- 최적값: balanced 결과 중 matched_n (matching) 또는 ESS (IPTW) 최대값

---

## compute_balance 주의사항

### Diff.Adj 컬럼

`cobalt::bal.tab()` 결과에서 SMD는 `"Diff.Adj"` 컬럼 사용. 컬럼 부재 시 NULL 반환.

```r
smd_vec <- abs(bal_df[["Diff.Adj"]])
# distance / prop.score 행 제거 (PS 변수 자체 제외)
smd_vec <- smd_vec[!names(smd_vec) %in% c("distance", "prop.score")]
```

### IPTW ESS

IPTW는 matched_n = nrow(df) (전체 사용), effective_n = ESS로 별도 계산:

```r
ess_t <- sum(w[t==1])^2 / sum(w[t==1]^2)
ess_c <- sum(w[t==0])^2 / sum(w[t==0]^2)
effective_n <- round(min(ess_t, ess_c))
```

---

## Claude API 설정

- 모델: `claude-opus-4-7`
- max_tokens: 1024
- 응답 형식: JSON only (시스템 프롬프트에 명시)
- 다중턴 대화: `messages` 리스트에 user/assistant 번갈아 누적

---

## 출력 파일

| 파일 | 내용 |
|------|------|
| `best_result.json` | 최적 설정 JSON (mode, treat, best_config, result_summary) |
| `.report_data.rds` | 보고서 렌더링용 중간 데이터 (report_template.Rmd에서 readRDS) |
| `report.html` | HTML 보고서 (요약, Balance Table, Love Plot, 탐색 이력) |

---

## 개발 이력

→ [PROGRESS.md](PROGRESS.md) 참조
