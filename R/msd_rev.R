#' Reverse Mean Signed Deviation
#'
#' @description
#' This metric computes the mean signed deviation as `estimate - truth`. It is a
#' thin wrapper around `yardstick::msd()`, returning the negative of `msd()`'s output, so that
#' the positive value indicate that `estimate` are overestimated.
#'
#' @details
#' By default, `yardstick::msd()` calculates `truth - estimate`. This "reverse" version
#' flips that sign, effectively computing `estimate - truth`.
#'
#' @export
msd_rev <- function(data, ...) {
  UseMethod("msd_rev")
}

msd_rev <- yardstick::new_numeric_metric(
  msd_rev,
  direction = "zero"
)

#' @rdname msd_rev
#' @export
msd_rev.data.frame <- function(data,
                               truth,
                               estimate,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  yardstick::numeric_metric_summarizer(
    name = "msd_rev",
    fn = msd_rev_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @rdname msd_rev
#' @export
msd_rev_vec <- function(truth,
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
