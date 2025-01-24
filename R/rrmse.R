#' Relative root mean squared error
#'
#' Calculate the relative root mean squared error. `rrmse()` expresses
#' the RMSE as a proportion of the range of the true values.
#'
#'
#' @param data A `data.frame` containing the columns specified by the `truth`
#' and `estimate` arguments.
#'
#' @param truth The column identifier for the true results
#' (that is `numeric`). This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#' results (that is also `numeric`). As with `truth` this can be
#' specified different ways but the primary method is to use an
#' unquoted variable name. For `_vec()` functions, a `numeric` vector.
#'
#' @param na_rm A `logical` value indicating whether `NA`
#' values should be stripped before the computation proceeds.
#'
#' @param case_weights Currently not implemented. 
#'
#' @param ... Not currently used.
#'
#' @export
#'

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
                             ...) {
  yardstick::numeric_metric_summarizer(
    name = "rrmse",
    fn = rrmse_vec,
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
  
  rrmse_impl(truth, estimate, case_weights = case_weights)
}

rrmse_impl <- function(truth, estimate, case_weights) {
  errors <- (truth - estimate)^2
  rmse_value <- sqrt(yardstick:::yardstick_mean(errors, case_weights = case_weights))
  range_truth <- (max(truth, na.rm = TRUE) - min(truth, na.rm = TRUE))
  
  if (range_truth == 0) {
    return(NA_real_)  # Avoid division by zero
  }
  
  rmse_value / range_truth * 100
}

