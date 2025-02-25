#' Reverse Mean Percentage Error
#'
#' @description
#' A thin wrapper around `yardstick::mpe()` that negates the final result, effectively
#' computing `(estimate - truth) / truth` rather than `(truth - estimate) / truth`.
#'
#'
#' @export
mpe_rev <- function(data, ...) {
  UseMethod("mpe_rev")
}

mpe_rev <- yardstick::new_numeric_metric(
  mpe_rev,
  direction = "zero"
)

#' @rdname mpe_rev
#' @export
mpe_rev.data.frame <- function(data,
                               truth,
                               estimate,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  yardstick::numeric_metric_summarizer(
    name = "mpe_rev",
    fn = mpe_rev_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @rdname mpe_rev
#' @export
mpe_rev_vec <- function(truth,
                        estimate,
                        na_rm = TRUE,
                        case_weights = NULL,
                        ...) {
  # Simply negate the result of mpe_vec()
  -mpe_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights,
    ...
  )
}
