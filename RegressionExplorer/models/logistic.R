# models/logistic.R  (모듈 파일에는 library() 없음)
.build_formula <- function(input, rv) {
  lhs  <- input$outcome
  vars <- unique(c(rv$force, rv$sel))
  if (length(vars) == 0) stats::as.formula(paste0(lhs, " ~ 1"))
  else stats::reformulate(vars, response = lhs)
}

.fit <- function(formula, data, input) {
  stats::glm(formula, data = data, family = stats::binomial(), model = TRUE)
}

.tidy <- function(model) {
  if (!requireNamespace("broom", quietly = TRUE)) stop("Package 'broom' is required.")
  broom::tidy(model, conf.int = FALSE)
}

.diagnostics <- function(model) {
  utils::capture.output(print(summary(model)))
}

.step <- function(base_fit, lower_form, upper_form, k, direction) {
  stats::step(base_fit, scope = list(lower = lower_form, upper = upper_form),
              direction = direction, k = k, trace = 0)
}

.model_logistic <- list(
  id="logistic", label="Logistic (Binomial)",
  time_based=FALSE, outcome_categ=TRUE, effect_name="OR",
  supports_step=TRUE, step_impl="stats_step",
  build_formula=.build_formula, fit=.fit, tidy=.tidy,
  diagnostics=.diagnostics, step=.step
)
