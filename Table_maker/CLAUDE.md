# CLAUDE.md — Table_maker

`gtsummary::tbl_summary()`를 래핑한 출판용 기술통계표 인터랙티브 변수 선택기.

## 실행

```r
shiny::runApp("Table_maker")
```

---

## 주요 기능

- Shapiro-Wilk로 정규성 자동 감지
- 연속변수: mean±SD (정규) 또는 median(IQR) (비정규)
- 범주형: 카이제곱 / Wilcoxon
- 상태(데이터 + 선택) JSON으로 저장/불러오기 가능
