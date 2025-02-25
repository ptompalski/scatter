#' Compute Agreement Metrics for Predictions
#'
#' This function calculates various metrics to assess the agreement between 
#' predicted and true values. It leverages the flexibility of the `yardstick` 
#' package to compute a variety of metrics, which can be specified through the 
#' `metrics` parameter.
#'
#' @param data A data frame containing the observed (`truth`) and predicted (`estimate`) values.
#' @param truth The column in `data` representing the true values. Use tidy evaluation (e.g., `{{}}`).
#' @param estimate The column in `data` representing the predicted values. Use tidy evaluation (e.g., `{{}}`).
#' @param metrics A list of metrics to compute. Metrics can be almost any function from the `yardstick` 
#'   package (e.g., `rsq`, `rmse`, `mape`, `msd`), provided they follow the `yardstick` format. Defaults to 
#'   `list(rsq, msd, mpe, rmse, rrmse)`. Can also be a named list of metrics to compute (e.g. `list(bias=msd, "bias%"=mpe)`.
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
#'
#' @export






agreement_metrics <- function(data, 
                               truth, 
                               estimate, 
                               metrics=list(rsq,
                                            msd,
                                            mpe,
                                            rmse,
                                            rrmse), 
                               label=FALSE) {
  
  
  #save metric names (if provided)
  custom_metrics_names<- names(metrics)
  
  
  metrics <- metric_set(!!!metrics) 
  
  
  # m <-
  #   data %>% 
  #   yardstick::metrics({{truth}}, {{estimate}}) %>%
  #   dplyr::select(-.estimator) 
  
  # Instead of calling yardstick::metrics(...), call the metric set:
  m <- metrics(data, truth = {{truth}}, estimate = {{estimate}})
  
  # Remove the .estimator column (usually "standard" or "macro" for classification metrics)
  m <- dplyr::select(m, -.estimator)
  
  
  metric_names <- unique(m$.metric)
  
  # print(metric_names)
  # print(custom_metrics_names)
  
  m <- 
    m %>%
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),~round(.x,2)) )
  
  
  #if custom_metric_names then rename
  if(!is.null(custom_metrics_names)) {
    
    custom_metrics_names[custom_metrics_names == ""] <- metric_names[custom_metrics_names == ""]
    
    colnames(m)[match(metric_names, colnames(m))] <- custom_metrics_names
    
    metric_names <- custom_metrics_names
  }
  
  
  if(label) {
    
    m<-
      m %>%
      dplyr::rowwise() %>%
      dplyr::mutate(label = paste(metric_names, as.character(dplyr::c_across(dplyr::all_of(metric_names))), sep = ": ", collapse = "; "))
    
  }
  return(m)
  
}
