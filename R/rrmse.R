#' Relative root mean squared error
#'
#' Calculate the relative root mean squared error. `rrmse()` expresses
#' the RMSE as a proportion of either the **mean** or **range** of the true values.
#'
#' @param data A `data.frame` containing the columns specified by the `truth`
#' and `estimate` arguments.
#'
#' @param truth The column identifier for the true results (numeric).
#'
#' @param estimate The column identifier for the predicted results (numeric).
#'
#' @param na_rm A `logical` value indicating whether `NA` values should be removed.
#'
#' @param case_weights Currently not implemented.
#'
#' @param normalization Character string specifying the normalization method:
#' `"mean"` (default) or `"range"`.
#'
#' @param ... Additional arguments passed to lower-level functions.
#'
#' @export
rrmse <- function(data, ...) {
  UseMethod("rrmse")
}

rrmse <- yardstick::new_numeric_metric(
  rrmse,
  direction = "minimize"
)

#' @rdname rrmse
#' @export
rrmse.data.frame <- function(data,
                             truth,
                             estimate,
                             na_rm = TRUE,
                             case_weights = NULL,
                             normalization = "mean",
                             ...) {
  normalization <- match.arg(normalization, choices = c("mean", "range"))
  
  metric_fn <- function(truth, estimate, na_rm = TRUE, case_weights = NULL) {
    rrmse_vec(
      truth = truth,
      estimate = estimate,
      na_rm = na_rm,
      case_weights = case_weights,
      normalization = normalization
    )
  }
  
  yardstick::numeric_metric_summarizer(
    name = "rrmse",
    fn = metric_fn,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rrmse
rrmse_vec <- function(truth,
                      estimate,
                      na_rm = TRUE,
                      case_weights = NULL,
                      normalization = "mean",
                      ...) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  normalization <- match.arg(normalization, choices = c("mean", "range"))
  rrmse_impl(truth, estimate, case_weights = case_weights, normalization = normalization)
}

rrmse_impl <- function(truth, estimate, case_weights, normalization = "mean") {
  errors <- (truth - estimate)^2
  rmse_value <- sqrt(yardstick:::yardstick_mean(errors, case_weights = case_weights))
  
  denom <- switch(
    normalization,
    mean = mean(truth, na.rm = TRUE),
    range = max(truth, na.rm = TRUE) - min(truth, na.rm = TRUE)
  )
  
  if (denom == 0) {
    return(NA_real_)
  }
  
  rmse_value / denom * 100
}
