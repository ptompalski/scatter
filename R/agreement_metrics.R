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
#'   `list(rsq, msd, rmse, mape)`.
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
                              metrics=list(yardstick::rsq,
                                           yardstick::msd,
                                           yardstick::rmse,
                                           yardstick::mape), 
                              label=FALSE) {
  
  metrics <- metric_set(!!!metrics) 
  
  m <-
    data %>% 
    metrics({{truth}}, {{estimate}}) %>%
    select(-.estimator) 
  
  metric_names <- unique(m$.metric)
  
  m <- 
    m %>%
    pivot_wider(names_from = .metric, values_from = .estimate) %>% 
    mutate(across(where(is.numeric),~round(.x,2)) )
  
  if(label) {
  
  ind <- length(names(m))-length(metric_names) #to determine how many columsn to skip
  
  m<-
    m %>%
    rowwise() %>%
    mutate(label = paste(metric_names, as.character(c_across(all_of(metric_names))), sep = ": ", collapse = "; "))
  
  }
  return(m)
  
}

# calc_metrics(data, truth = observed, estimate = predicted, metrics = list(rsq, rmse, mape), label = T)
# 
# 
# data %>% group_by(group) %>%
# calc_metrics(truth = observed, estimate = predicted, metrics = list(rsq, rmse, mape, mpe, huber_loss), label = T)
# 
# 
# m <- calc_metrics(data, truth = observed, estimate = predicted, metrics = list(rsq, rmse, mape), label = F)
# metric_names <- names(m)
# 
# 
# m %>%
#   rowwise() %>%
#   mutate(label = paste(metric_names, as.character(c_across(all_of(metric_names))), sep = ": ", collapse = "; "))
