# WHERE: models/cox.R (new file)
# WHY: Cox PH 회귀 모듈화(stepAIC 사용), Surv 좌변 빌드 포함
# <<<INSERT START>>>


.build_formula <- function(input, rv) {
  lhs  <- paste0("Surv(", input$time_col, ", ", input$event_col, ")")
  vars <- unique(c(rv$force, rv$sel))
  if (length(vars) == 0) as.formula(paste0(lhs, " ~ 1"))
  else as.formula(paste0(lhs, " ~ ", paste(vars, collapse = " + ")))
}

.fit <- function(formula, data, input) {
  survival::coxph(formula, data = data, model = TRUE)
}

.tidy <- function(model) broom::tidy(model, conf.int = FALSE)

.diagnostics <- function(model) {
  out <- tryCatch({
    capture.output(print(survival::cox.zph(model)))
  }, error = function(e) paste("cox.zph failed:", e$message))
  out
}

.step <- function(base_fit, lower_form, upper_form, k, direction) {
  MASS::stepAIC(base_fit,
                scope = list(lower = lower_form, upper = upper_form),
                direction = direction, k = k, trace = FALSE)
}

.model_cox <- list(
  id = "cox",
  label = "Cox PH (Survival)",
  time_based = TRUE,
  outcome_categ = FALSE,
  effect_name = "HR",
  supports_step = TRUE,
  step_impl = "stepAIC",
  build_formula = .build_formula,
  fit = .fit,
  tidy = .tidy,
  diagnostics = .diagnostics,
  step = .step
)
# <<<INSERT END>>>