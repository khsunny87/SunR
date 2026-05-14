# CLAUDE.md — RegressionExplorer

단변량 선별·VIF 진단·단계적 선택이 포함된 모듈형 회귀분석 Shiny 앱.

## 실행

```r
shiny::runApp("RegressionExplorer")
```

---

## 아키텍처

```
settings.json → model_registry.R (ModelRegistry env)
                      ↓
              app.R (반응형 상태)
```

`model_registry.R`이 `settings.json`을 로드하고 각 모델 파일을 `ModelRegistry` 환경에 `sys.source()`함. `get_model(id)`, `run_stepwise()` 제공.

### 반응형 상태

- `rv$sel`: 선택된 공변량 벡터
- `rv$force`: 강제 포함 공변량 벡터
- `fit_store`: 적합된 모델 객체
- 체크박스 이벤트(`sel_changed`, `force_changed`): DT 테이블 내 인라인 JS에서 발생
- 유의한 행(p < 0.05): `highlight_sig_rows()`로 노란색/빨간색 강조

---

## 새 회귀 모델 추가 패턴

1. `models/<id>.R` 생성 — 필수 필드를 가진 `.model_<id>` 리스트 export:

   | 필드 | 타입 | 설명 |
   |------|------|------|
   | `id` | string | 모델 식별자 |
   | `label` | string | UI 표시명 |
   | `time_based` | logical | 생존분석 여부 |
   | `effect_name` | string | 효과크기 컬럼명 (예: "OR", "HR") |
   | `supports_step` | logical | 단계적 선택 지원 여부 |
   | `build_formula(input, rv)` | function | → formula |
   | `fit(formula, data, input)` | function | → model object |
   | `tidy(model)` | function | → broom 스타일 tibble |
   | `diagnostics(model)` | function | → `list(model=..., plot=list(...))` |
   | `step(...)` | function | → stepped model |

2. `settings.json`에 항목 추가:
   ```json
   { "id": "<id>", "label": "<표시명>", "file": "models/<id>.R" }
   ```

### 현재 등록된 모델

| id | label |
|----|-------|
| `linear` | Linear (Gaussian) |
| `logistic` | Logistic (Binomial) |
| `cox` | Cox PH (Survival) |
