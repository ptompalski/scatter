#' Scatterplot with Observed and Predicted Values
#'
#' This function creates a scatterplot comparing observed and predicted values.
#' It supports grouped data, adding facets for each group, and optionally includes 
#' agreement metrics as text annotations in the plot.
#'
#' @param data A data frame or tibble. Can be grouped (using `dplyr::group_by`) to create faceted plots.
#' @param observed The column name in `data` containing observed values. Should be unquoted.
#' @param predicted The column name in `data` containing predicted values. Should be unquoted.
#' @param add_metrics Logical. If `TRUE`, agreement metrics (R², bias, RMSE) will be added 
#'   as text annotations to the plot. For grouped data, metrics are calculated and displayed for each group.
#' @param r2_method A character string indicating the method for calculating \(R^2\). 
#'   Options are `"sum_of_squares"` (default) for the traditional \(R^2\) based on 
#'   residual and total sums of squares, or `"correlation"` for the square of the 
#'   Pearson correlation coefficient between observed and predicted values.
#'
#' @details 
#' The function dynamically calculates axis ranges based on the observed and predicted values 
#' and ensures the plot is square by using `coord_fixed()`. For grouped data, it uses `facet_wrap` 
#' to create separate scatterplots for each group. 
#' 
#' Agreement metrics are calculated using the `agreement_metrics` function.
#'
#' @return A ggplot object.
#'
#'
#' @examples
#' # Example data
#' set.seed(123)
#' df <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 50),
#'   observed = c(rnorm(50, 10, 2), rnorm(50, 20, 3), rnorm(50, 15, 4)),
#'   predicted = c(rnorm(50, 10, 2), rnorm(50, 20, 3), rnorm(50, 15, 4))
#' )
#'
#' # Simple scatterplot
#' scatter(df, observed, predicted)
#'
#' # Scatterplot with agreement metrics
#' scatter(df, observed, predicted, add_metrics = TRUE)
#'
#' # Grouped scatterplot with agreement metrics
#' df %>% 
#' group_by(group) %>%
#' scatter(observed, predicted, add_metrics = TRUE)
#'
#' @export
#' 
scatter <- function(data, observed, predicted, add_metrics = FALSE,  r2_method = c("sum_of_squares", "correlation")) {
  # Ensure the observed and predicted columns exist
  if (!all(c(as.character(substitute(observed)), as.character(substitute(predicted))) %in% colnames(data))) {
    stop("The specified observed and predicted variables do not exist in the data.")
  }
  
  # Match the method argument
  r2_method <- match.arg(r2_method)
  
  # Check if the data is grouped
  is_grouped <- dplyr::is_grouped_df(data)
  
  # Calculate the combined range of observed and predicted values
  range_values <- range(data[[as.character(substitute(observed))]],
                        data[[as.character(substitute(predicted))]],
                        na.rm = TRUE)
  
  # Start building the plot
  p <- ggplot(data, aes(y = {{observed}}, x = {{predicted}})) +
    geom_point(shape = 1, size = 2) +  
    geom_abline(intercept = 0, slope = 1, color = "grey50") +  # Add 1:1 line
    labs(
      y = as.character(substitute(observed)),
      x = as.character(substitute(predicted))
    ) +
    coord_fixed(xlim = range_values, ylim = range_values) +  # Ensure equal axis limits and square plot
    theme_baseR()  # Apply custom theme
  
  # Add faceting if the data is grouped
  if (is_grouped) {
    facet_vars <- dplyr::group_vars(data)
    p <- p + facet_wrap(vars(!!!rlang::syms(facet_vars)), scales = "fixed")
  }
  
  if(add_metrics & !is_grouped) {
    metrics <- agreement_metrics(data = data, observed = {{observed}}, predicted = {{predicted}}, add_overall = FALSE, r2_method = r2_method) 
    
    # Create a formatted text string for the textbox
    metrics_text <- paste0(
      "R²: ", round(metrics$R2, 2),"\n",
      "bias: ", round(metrics$bias, 2), " (",round(metrics$bias_perc, 2), "%)\n",
      "RMSE: ", round(metrics$RMSE, 2), " (",round(metrics$RMSE_perc, 2), "%)"
    )
    
    ann_x <- min(range_values)
    ann_y <- max(range_values)
    ann_hjust <- 0
    ann_vjust <- 0.9
    
    ann <- ggplot2::annotate("text", 
                             x = ann_x, 
                             y = ann_y, 
                             label = metrics_text, 
                             hjust = ann_hjust, 
                             vjust = ann_vjust)
    
    # Add the textbox to the plot
    p <- p + ann
  }
  
  if(add_metrics & is_grouped) {
    metrics <- agreement_metrics(data = data, observed = {{observed}}, predicted = {{predicted}}, add_overall = F,  r2_method = r2_method) %>%
      mutate(metrics_text = paste0(
        "R²: ", round(R2, 2),"\n",
        "bias: ", round(bias, 2), " (",round(bias_perc, 2), "%)\n",
        "RMSE: ", round(RMSE, 2), " (",round(RMSE_perc, 2), "%)"
      ))
    
    p <- p + geom_text(
      data = metrics,
      aes(x = min(range_values), y = max(range_values), label = metrics_text),
      hjust = 0, vjust = 1, inherit.aes = FALSE, size = 3
    )
  }
  
  return(p)
}
