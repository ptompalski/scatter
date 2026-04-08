#' Relative Mean Difference (RMD)
#'
#' @description
#' Computes the relative mean signed deviation between `estimate` and `truth`,
#' calculated as the mean of (`estimate - truth`) divided by the mean of `truth`,
#' expressed as a percentage. Positive values indicate overestimation.
#'
#' This is a thin wrapper around `yardstick::msd()`, with reversed sign and
#' scaled by the mean of the true values. The default `yardstick::msd()` computes
#' `truth - estimate`, while this function computes `estimate - truth`.
#'
#' @details
#' The relative mean difference is useful when comparing average deviations
#' across models or datasets, as it expresses the difference relative to the
#' size of the true value. It supports optional case weights and handles missing
#' values similarly to other `yardstick` metrics.
#'
#' @param data A data frame containing the `truth` and `estimate` columns.
#' @param truth The column identifier for the true values (bare or quoted).
#' @param estimate The column identifier for the predicted values (bare or quoted).
#' @param na_rm A logical value indicating whether `NA` values should be removed before computation.
#' @param case_weights An optional column of case weights.
#' @param ... Additional arguments passed to lower-level functions.
#'
#' @export
rmd <- function(data, ...) {
  UseMethod("rmd")
}

rmd <- yardstick::new_numeric_metric(
  rmd,
  direction = "zero"
)

#' @rdname rmd
#' @export
rmd.data.frame <- function(data,
                               truth,
                               estimate,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  yardstick::numeric_metric_summarizer(
    name = "rmd",
    fn = rmd_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights)
  )
}

#' @rdname rmd
#' @export
rmd_vec <- function(truth,
                        estimate,
                        na_rm = TRUE,
                        case_weights = NULL,
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

  rmd_impl(truth, estimate, case_weights = case_weights)
  
  
}


rmd_impl <- function(truth, estimate, case_weights) {
  
  weighted_mean <- function(x, w) {
    if (is.null(w)) mean(x) else stats::weighted.mean(x, w)
  }

  mean_difference <- weighted_mean(estimate - truth, case_weights)
  mean_truth <- weighted_mean(truth, case_weights)
  
  out <- mean_difference / mean_truth * 100
  
  out
}


