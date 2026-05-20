library(gtsummary)
library(survey)
library(smd)
library(dplyr)

# make_table1: gtsummary Table 1 with survey-based SMD
#   (identical to PS_Explorer compute_balance_stats SMD)
#
# Args:
#   df      : data frame
#   treat   : treatment column name (string)
#   covs    : covariate names vector
#   weights : IPTW weight vector (NULL = unweighted / matching)
#   digits  : decimal places for continuous variables (default 1)
#
# Returns: gtsummary tbl_summary or tbl_svysummary object
#
# Usage:
#   source("table1.R")
#
#   # Unweighted (before IPTW / unadjusted)
#   t1_before <- make_table1(df, treat = "DCD", covs = covs)
#
#   # IPTW-weighted (after IPTW)
#   t1_after  <- make_table1(df, treat = "DCD", covs = covs, weights = iptw_weights)
#
#   # Merge before/after into one table
#   tbl_merge(list(t1_before, t1_after),
#             tab_spanner = c("**Before IPTW**", "**After IPTW**"))

make_table1 <- function(df, treat, covs, weights = NULL, digits = 1) {

  .smd_stat <- function(data, variable, by, ...) {
    if (inherits(data, "survey.design")) {
      w        <- weights(data)
      df_inner <- data$variables
    } else {
      w        <- data[[".__w__"]]
      df_inner <- data
    }
    x  <- df_inner[[variable]]
    gc <- as.character(df_inner[[by]])
    complete <- !is.na(x) & !is.na(gc)
    xc <- x[complete]; gc <- gc[complete]; wc <- w[complete]
    grp     <- sort(unique(gc))
    is_cont <- is.numeric(xc)
    g1 <- gc == grp[1]; g2 <- gc == grp[2]

    val <- tryCatch({
      if (is_cont) {
        .cg <- function(v, ws) {
          d <- survey::svydesign(ids = ~1, data = data.frame(x = v), weights = ~ws)
          list(m = as.numeric(survey::svymean(~x, d)),
               v = as.numeric(survey::svyvar(~x, d)))
        }
        s1 <- .cg(xc[g1], wc[g1]); s2 <- .cg(xc[g2], wc[g2])
        abs(s1$m - s2$m) / sqrt((s1$v + s2$v) / 2)
      } else {
        abs(smd::smd(x = xc, g = factor(gc), w = wc, na.rm = FALSE)$estimate)
      }
    }, error = function(e) NA_real_)

    tibble::tibble(`|SMD|` = ifelse(is.na(val), NA_character_, sprintf("%.3f", val)))
  }

  if (!is.null(weights)) {
    svy <- survey::svydesign(ids = ~1, data = df, weights = ~ weights)
    tbl_svysummary(
      data    = svy,
      by      = all_of(treat),
      include = all_of(covs),
      digits  = list(all_continuous() ~ digits)
    ) |>
      add_overall() |>
      add_stat(fns = everything() ~ .smd_stat) |>
      modify_header(add_stat_1 ~ "**|SMD|**") |>
      bold_labels()
  } else {
    df[[".__w__"]] <- 1
    tbl_summary(
      data    = df |> select(all_of(c(treat, covs, ".__w__"))),
      by      = all_of(treat),
      include = all_of(covs),
      digits  = list(all_continuous() ~ digits)
    ) |>
      add_overall() |>
      add_stat(fns = everything() ~ .smd_stat) |>
      modify_header(add_stat_1 ~ "**|SMD|**") |>
      bold_labels()
  }
}
