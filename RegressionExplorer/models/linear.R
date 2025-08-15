

.build_formula <- function(input, rv) {
  lhs  <- input$outcome
  vars <- unique(c(rv$force, rv$sel))
  if (length(vars) == 0) as.formula(paste0(lhs, " ~ 1")) else reformulate(vars, response = lhs)
}

.fit <- function(formula, data, input) {
  glm(formula, data = data, family = stats::gaussian(), model = TRUE)
}

.tidy <- function(model) broom::tidy(model, conf.int = FALSE)

.diagnostics <- function(model) capture.output(print(summary(model)))

.step <- function(base_fit, lower_form, upper_form, k, direction) {
  MASS::stepAIC(base_fit,
                scope = list(lower = lower_form, upper = upper_form),
                direction = direction, k = k, trace = FALSE)
}

.model_linear <- list(
  id = "linear",
  label = "Linear (Gaussian)",
  time_based = FALSE,
  outcome_categ = FALSE,
  effect_name = "\u03B2",
  supports_step = TRUE,
  step_impl = "stepAIC",
  build_formula = .build_formula,
  fit = .fit,
  tidy = .tidy,
  diagnostics = .diagnostics,
  step = .step
)