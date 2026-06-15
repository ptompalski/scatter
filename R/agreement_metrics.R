#' Compute Agreement Metrics for Predictions
#'
#' This function calculates various metrics to assess the agreement between 
#' predicted and true values. It leverages the flexibility of the `yardstick` 
#' package to compute a variety of metrics, which can be specified through the 
#' `metrics` parameter.
#'
#' @param data A data frame containing the observed (`truth`) and predicted (`estimate`) values.
#' @param truth The column in `data` representing the true values. Use a bare column name.
#' @param estimate The column in `data` representing the predicted values. Use a bare column name.
#' @param metrics A list of metrics to compute. Metrics can be almost any function from the `yardstick`
#'   package (e.g., `rsq`, `rmse`, `mape`, `msd`), provided they
#'   follow the `yardstick` format. Metrics can also be wrapped with
#'   `metric_format()` or paired with `metric_pair()` to control labels.
#'   Can also be a named list of metrics to compute (e.g.
#'   `list(bias = metric_pair(md, rmd), RMSE = yardstick::rmse)`).
#'   Names are then used instead of the `yardstick` metric names.
#' @param label Logical. If `TRUE`, the function creates a concatenated string summarizing all
#'   computed metrics in a new column called `label`. Defaults to `FALSE`.
#'
#' @return A data frame with the computed metrics. If `label = TRUE`, the output includes a `label` column 
#' summarizing all metrics.
#'
#' @details 
#' The function internally uses the `metric_set` function from `yardstick` to handle 
#' the specified metrics. The results are returned in a wide format, with each metric 
#' as a column. All numeric metrics are rounded to two decimal places for readability.
#'
#' @examples
#' library(dplyr)
#' library(yardstick)
#' library(tidyr)
#'
#' # Example dataset
#' data <- data.frame(
#'   truth = c(3, 5, 2.5, 7),
#'   estimate = c(2.8, 5.1, 2.6, 7.2)
#' )
#'
#' # Compute default metrics
#' agreement_metrics(data, truth = truth, estimate = estimate)
#'
#' agreement_metrics(
#'   data,
#'   truth = truth,
#'   estimate = estimate,
#'   metrics = list(
#'     RMSE = metric_pair(yardstick::rmse, rrmse, "{value:.1f} ({percent:.0f}%)")
#'   ),
#'   label = TRUE
#' )
#'
#'
#' @export






agreement_metrics <- function(data,
                              truth,
                              estimate,
                              metrics = list(
                                "R\u00B2" = yardstick::rsq,
                                "bias" = metric_pair(md, rmd),
                                "RMSE" = metric_pair(yardstick::rmse, rrmse)
                              ),
                              label = FALSE) {
  
  
  metric_display <- prepare_metric_display(metrics)
  metrics <- metric_display$metrics
  custom_metrics_names <- metric_display$names
  
  
  metrics <- yardstick::metric_set(!!!metrics)
  
  
  # m <-
  #   data %>% 
  #   yardstick::metrics({{truth}}, {{estimate}}) %>%
  #   dplyr::select(-.estimator) 
  
  # Instead of calling yardstick::metrics(...), call the metric set:
  m <- metrics(data, truth = {{truth}}, estimate = {{estimate}})
  
  # Remove the .estimator column (usually "standard" or "macro" for classification metrics)
  m <- dplyr::select(m, -dplyr::all_of(".estimator"))
  
  
  metric_names <- unique(m[[".metric"]])
  
  # print(metric_names)
  # print(custom_metrics_names)
  
  m <- 
    m %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(".metric"), values_from = dplyr::all_of(".estimate")) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),~round(.x,2)) )
  
  
  #if custom_metric_names then rename
  custom_metrics_names[custom_metrics_names == ""] <- metric_names[custom_metrics_names == ""]

  colnames(m)[match(metric_names, colnames(m))] <- custom_metrics_names

  metric_names <- custom_metrics_names
  label_specs <- finalize_metric_display_specs(metric_display$label_specs, metric_names)
  
  
  if(label) {
    
    m$label <- build_metric_labels(m, label_specs)
    
  }
  return(m)
  
}
