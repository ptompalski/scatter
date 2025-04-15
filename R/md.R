#' Mean Difference (Mean Signed Deviation)
#'
#' @description
#' Computes the mean signed deviation between `estimate` and `truth`, calculated as
#' the mean of (`estimate - truth`). This is a wrapper around `yardstick::msd()` that
#' reverses the default sign, so positive values indicate overestimation.
#'
#' @details
#' `yardstick::msd()` computes the mean signed deviation as `truth - estimate`, which means
#' positive values indicate underestimation. This metric reverses the sign by returning
#' `-(truth - estimate)`, effectively calculating `estimate - truth`.
#'
#' This metric is useful for assessing average directional bias in predictions.
#' Like other `yardstick` metrics, it supports missing value handling and case weights.
#'
#' @param data A data frame containing the `truth` and `estimate` columns.
#' @param truth The column identifier for the true (observed) values (bare or quoted).
#' @param estimate The column identifier for the predicted (estimated) values (bare or quoted).
#' @param na_rm A logical value indicating whether `NA` values should be removed before computation.
#' @param case_weights An optional column of case weights.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A single numeric value representing the mean signed deviation (`estimate - truth`).
#'
#' @export
md <- function(data, ...) {
  UseMethod("md")
}

md <- yardstick::new_numeric_metric(
  md,
  direction = "zero"
)

#' @rdname md
#' @export
md.data.frame <- function(data,
                               truth,
                               estimate,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  yardstick::numeric_metric_summarizer(
    name = "md",
    fn = md_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @rdname md
#' @export
md_vec <- function(truth,
                        estimate,
                        na_rm = TRUE,
                        case_weights = NULL,
                        ...) {
  # Simply negate the result of msd_vec()
  -yardstick::msd_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights,
    ...
  )
}
