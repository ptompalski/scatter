#' Count Observations
#'
#' @description
#' Counts the number of paired observations used to calculate numeric agreement
#' metrics. By default, missing `truth`, `estimate`, or `case_weights` values are
#' removed before counting, matching the missing-value behavior of other
#' `yardstick` numeric metrics.
#'
#' @param data A data frame containing the `truth` and `estimate` columns.
#' @param truth The column identifier for the true values (bare or quoted).
#' @param estimate The column identifier for the predicted values (bare or quoted).
#' @param na_rm A logical value indicating whether `NA` values should be removed before counting.
#' @param case_weights An optional column of case weights. These are only used for
#' missing-value filtering; the returned value is the row count, not the sum of weights.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A single numeric value representing the number of paired observations.
#'
#' @export
n_obs <- function(data, ...) {
  UseMethod("n_obs")
}

n_obs <- yardstick::new_numeric_metric(
  n_obs,
  direction = "maximize"
)

#' @rdname n_obs
#' @export
n_obs.data.frame <- function(data,
                             truth,
                             estimate,
                             na_rm = TRUE,
                             case_weights = NULL,
                             ...) {
  yardstick::numeric_metric_summarizer(
    name = "n",
    fn = n_obs_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights)
  )
}

#' @rdname n_obs
#' @export
n_obs_vec <- function(truth,
                      estimate,
                      na_rm = TRUE,
                      case_weights = NULL,
                      ...) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  length(truth)
}
