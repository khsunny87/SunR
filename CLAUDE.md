# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

SunR is a collection of R tools for clinical/medical research statistics. It includes two Shiny web applications and several utility R scripts for survival analysis, regression, forest plots, CONSORT diagrams, and IPTW.

## Package Management

renv is used for dependency management. The `.Rprofile` auto-activates renv.

```r
renv::restore()   # restore all packages from renv.lock
renv::snapshot()  # update renv.lock after adding packages
```

R version: 4.5.2

## Running the Shiny Apps

```r
shiny::runApp("RegressionExplorer")   # Interactive Regression Workbench
shiny::runApp("Table_maker")          # Variable Picker for Publication Tables
```

## Architecture

### RegressionExplorer/

A modular Shiny app for interactive regression analysis with univariable screening, VIF diagnosis, and stepwise selection.

**Extension pattern** — adding a new regression model:
1. Create `RegressionExplorer/models/<id>.R` exporting a `.model_<id>` list with these required fields:
   - `build_formula(input, rv)` → formula
   - `fit(formula, data, input)` → model object
   - `tidy(model)` → broom-style tibble
   - `diagnostics(model)` → `list(model=..., plot=list(list(title=..., draw=function()...)))`
   - `step(base_fit, lower_form, upper_form, k, direction)` → stepped model
   - `id`, `label`, `time_based`, `effect_name`, `supports_step`, `step_impl`
2. Add an entry to `RegressionExplorer/settings.json`.

`model_registry.R` loads `settings.json`, `sys.source()`s each model file into `ModelRegistry` env, and provides `get_model(id)` and `run_stepwise()`.

`app.R` reactive state: `rv$sel` (selected covariates), `rv$force` (force-in covariates), `fit_store` (fitted model). Checkbox events (`sel_changed`, `force_changed`) come from inline JS in the DT table. Significant rows (p < 0.05) are highlighted yellow/red via `highlight_sig_rows()`.

### Table_maker/

Shiny app that wraps `gtsummary::tbl_summary()` with an interactive variable picker. Normality is auto-detected via Shapiro-Wilk; continuous variables use mean±SD or median(IQR) accordingly, categorical variables use chi-square or Wilcoxon tests. State (data + selections) can be saved/loaded as JSON.

### Utility Scripts

| File | Purpose |
|------|---------|
| `Sun_Survival.R` | `Get_KM2()` for KM curves, `Get_CMP()` for competing risks, `UV_Cox()` / `MV_Cox()` for Cox regression pipelines |
| `Sun_forest.R` | `Forest_df(model)` extracts broom tidy output into forest-plot-ready data frame; `Plot_forest(plot_df)` renders with ggplot2 |
| `Sun_CONSORT.R` | DiagrammeR-based CONSORT flow diagrams via grid node/edge helpers |
| `IPTW.R` | Propensity score IPTW workflow using `MatchIt` + `survey` + `gtsummary` with custom SMD statistic |
| `euroscore.R` | EuroSCORE II cardiac surgery risk calculation |
