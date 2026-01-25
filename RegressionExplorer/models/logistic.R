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

  ## ===============================
  ## 0. SAFE response extraction
  ## ===============================
  mf <- model$model
  resp <- all.vars(formula(model))[1]

  y_raw <- mf[[resp]]
  y <- as.integer(y_raw %in% c(1, "1", TRUE, "TRUE"))

  p_hat <- stats::predict(model, type = "response")

  df_plot <- mf
  df_plot$.y    <- y
  df_plot$.pred <- p_hat

  ## ===============================
  ## 1. GLOBAL Likelihood Ratio Test
  ## ===============================
  null_fit <- stats::glm(
    stats::update.formula(formula(model), . ~ 1),
    data   = mf,
    family = stats::binomial()
  )

  ll_null <- as.numeric(stats::logLik(null_fit))
  ll_full <- as.numeric(stats::logLik(model))

  LR  <- 2 * (ll_full - ll_null)
  df  <- attr(stats::logLik(model), "df") -
         attr(stats::logLik(null_fit), "df")
  p_LR <- stats::pchisq(LR, df = df, lower.tail = FALSE)

  model_rows <- list(
    data.frame(
      Test = "Likelihood ratio (GLOBAL)",
      Chisq = LR,
      df = df,
      `Pr(>Chisq)` = p_LR,
      check.names = FALSE
    )
  )

  ## ===============================
  ## 2. Hosmer–Lemeshow (OPTIONAL)
  ## ===============================
  if (requireNamespace("ResourceSelection", quietly = TRUE)) {
    hl_row <- tryCatch({
hl <- ResourceSelection::hoslem.test(
  y,
  p_hat,
  g = min(10, max(2, floor(length(y) / 50)))
)


      data.frame(
        Test = "Hosmer–Lemeshow (GLOBAL)",
        Chisq = unname(hl$statistic),
        df = unname(hl$parameter),
        `Pr(>Chisq)` = unname(hl$p.value),
        check.names = FALSE
      )
    }, error = function(e) {
      data.frame(
        Test = "Hosmer–Lemeshow (FAILED)",
        Chisq = NA,
        df = NA,
        `Pr(>Chisq)` = e$message,
        check.names = FALSE
      )
    })

    model_rows <- c(model_rows, list(hl_row))
  }

  model_table <- do.call(rbind, model_rows)

  ## ===============================
  ## 3. ROC + AUC (95% CI)
  ## ===============================
  plots <- list()

  if (requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(y, p_hat, quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    auc_ci  <- pROC::ci.auc(roc_obj)

    plots <- c(
      plots,
      list(list(
        title = "ROC curve",
        draw = function() {
          plot(roc_obj, col = "blue", lwd = 2)
          legend(
            "bottomright",
            legend = sprintf(
              "AUC = %.3f (95%% CI %.3f–%.3f)",
              auc_val, auc_ci[1], auc_ci[3]
            ),
            bty = "n"
          )
        }
      ))
    )
  }

  ## ===============================
  ## 4. Calibration plots
  ## ===============================
  vars <- names(model$coefficients)
  vars <- vars[vars != "(Intercept)"]

  plots <- c(
    plots,
    lapply(vars, function(v) {
      list(
        title = paste("Calibration (smoothed):", v),
        draw = function() {
          lo <- loess(.y ~ .pred, data = df_plot)
          ord <- order(df_plot$.pred)

          plot(
            df_plot$.pred, df_plot$.y,
            pch = 16, cex = 0.6,
            xlab = "Predicted probability",
            ylab = "Observed outcome"
          )
          lines(
            df_plot$.pred[ord],
            predict(lo)[ord],
            col = "red", lwd = 2
          )
          abline(0, 1, lty = 2)
        }
      )
    })
  )

  ## ===============================
  ## 5. RETURN (절대 NULL 아님)
  ## ===============================
  list(
    model = model_table,
    plot  = plots
  )
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
