# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 개요

SunR은 임상/의학 연구 통계를 위한 R 도구 모음입니다. Shiny 웹 앱 3개와 생존분석·회귀분석·산림 플롯·CONSORT 다이어그램·IPTW 유틸리티 스크립트로 구성됩니다.

## 패키지 관리

`renv`로 의존성 관리. `.Rprofile`이 자동 활성화.

```r
renv::restore()   # renv.lock에서 패키지 복원
renv::snapshot()  # 패키지 추가 후 renv.lock 갱신
```

R 버전: 4.5.2

## Shiny 앱

| 앱 | 설명 | 세부 가이드 |
|----|------|------------|
| `RegressionExplorer` | 인터랙티브 회귀분석 워크벤치 | [RegressionExplorer/CLAUDE.md](RegressionExplorer/CLAUDE.md) |
| `Table_maker` | 출판용 기술통계표 변수 선택기 | [Table_maker/CLAUDE.md](Table_maker/CLAUDE.md) |
| `PS_Explorer` | 성향점수 분석 (매칭 + IPTW) ← 주요 개발 중 | [PS_Explorer/CLAUDE.md](PS_Explorer/CLAUDE.md) |

## 유틸리티 스크립트

| 파일 | 역할 |
|------|------|
| `Sun_Survival.R` | `Get_KM2()` KM 곡선, `Get_CMP()` 경쟁위험, `UV_Cox()`/`MV_Cox()` Cox 회귀 파이프라인 |
| `Sun_forest.R` | `Forest_df(model)` broom tidy → 산림플롯 데이터프레임; `Plot_forest(plot_df)` ggplot2 렌더링 |
| `Sun_CONSORT.R` | DiagrammeR 기반 CONSORT 흐름도 (그리드 노드/엣지 헬퍼) |
| `IPTW.R` | 참조용 IPTW 워크플로 — `WeightIt` + `survey` + `gtsummary` 수동 구현 예시 |
| `euroscore.R` | EuroSCORE II 심장수술 위험도 계산 |
