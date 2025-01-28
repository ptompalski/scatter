#' Scatterplot with Truth and Estimate Values
#'
#' This function creates a scatterplot comparing `truth` and `estimate` values. It supports grouped data, 
#' adding facets for each group, and can optionally include agreement metrics as text annotations in the plot. 
#' Metrics can be positioned either inside the plot area or outside as subtitles or facet labels.
#'
#' @param data A data frame or tibble. Can be grouped (using `dplyr::group_by`) to create faceted plots.
#' @param truth The column name in `data` containing truth values. Should be unquoted.
#' @param estimate The column name in `data` containing estimate values. Should be unquoted.
#' @param metrics A list of metrics to compute and display. Metrics can include almost any function from the `yardstick` package 
#'   (e.g., `rsq`, `rmse`, `mape`). Defaults to `list(rsq, msd, mpe, rmse, rrmse)`. Set to `NULL` to disable.
#' @param metrics_position A character string indicating where to display metrics. Options are `"inside"` 
#'   (as annotations within the plot) or `"outside"` (as subtitle or facet labels). Defaults to `"inside"`.
#' @param metrics_inside_placement A character string indicating the position of the metrics within the plot. 
#'   Options are "upperright", "upperleft", "lowerright", or "lowerleft". Defaults to "upperright".
#'
#' @details
#' The function dynamically calculates axis ranges based on the `truth` and `estimate` values, ensuring a square plot using 
#' `coord_fixed()`. For grouped data, it uses `facet_wrap` to create separate scatterplots for each group.
#'
#' Agreement metrics are calculated using the `agreement_metrics` function, and their display depends on the value of 
#' `metrics_position`. For grouped data with `metrics_position = "outside"`, metrics are added to the facet labels, 
#' while for `metrics_position = "inside"`, they are displayed within each plot as annotations.
#'
#' @return A ggplot object.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # Example data
#' set.seed(123)
#' df <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 50),
#'   truth = c(rnorm(50, 10, 2), rnorm(50, 20, 3), rnorm(50, 15, 4)),
#'   estimate = c(rnorm(50, 10, 2), rnorm(50, 20, 3), rnorm(50, 15, 4))
#' )
#'
#' # Simple scatterplot
#' scatter(df, truth, estimate)
#'
#' # Scatterplot with agreement metrics (inside plot)
#' scatter(df, truth, estimate, metrics = list(rsq, mape))
#'
#' # Scatterplot with agreement metrics (outside plot as subtitle)
#' scatter(df, truth, estimate, metrics = list(rsq, rmse), metrics_position = "outside")
#'
#' # Grouped scatterplot with agreement metrics inside
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate, metrics = list(rsq,rmse,rrmse), metrics_position = "inside")
#'
#' # Grouped scatterplot with agreement metrics outside as facet labels
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate, metrics = list(rsq, rmse), metrics_position = "outside")
#'
#' @export
scatter <- function(data, truth, estimate, 
                    metrics = list(rsq, msd, mpe, rmse, rrmse),
                    metrics_position = "inside",
                    metrics_inside_placement = "upperleft") {
  
  # Ensure the truth and estimate columns exist
  if (!all(c(as.character(substitute(truth)), as.character(substitute(estimate))) %in% colnames(data))) {
    stop("The specified truth and estimate variables do not exist in the data.")
  }
  
  # Check if any metrics provided
  add_metrics <- ifelse(!is.null(metrics), TRUE, FALSE)
  
  # Check if the data is grouped
  is_grouped <- dplyr::is_grouped_df(data)
  
  # Calculate the combined range of truth and estimate values
  range_values <- range(data[[as.character(substitute(truth))]],
                        data[[as.character(substitute(estimate))]],
                        na.rm = TRUE)
  
  # Start building the plot
  p <- 
    ggplot2::ggplot(data, aes(y = {{truth}}, x = {{estimate}})) +
    ggplot2::geom_point(shape = 1, size = 2) +  
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "grey50") +  # Add 1:1 line
    ggplot2::labs(
      y = as.character(substitute(truth)),
      x = as.character(substitute(estimate))
    ) +
    ggplot2::coord_fixed(xlim = range_values, ylim = range_values) +  # Ensure equal axis limits and square plot
    theme_baseR()  # Apply custom theme
  
  # Add faceting if the data is grouped
  if (is_grouped) {
    facet_vars <- dplyr::group_vars(data)
    groups_count <- length(facet_vars)
    p <- p + ggplot2::facet_wrap(vars(!!!rlang::syms(facet_vars)), scales = "fixed")
  }
  
  # Determine annotation position based on metrics_inside_placement
  position_coords <- list(
    upperright = c(max(range_values), max(range_values)),
    upperleft = c(min(range_values), max(range_values)),
    lowerright = c(max(range_values), min(range_values)),
    lowerleft = c(min(range_values), min(range_values))
  )
  
  if (metrics_inside_placement %in% names(position_coords)) {
    ann_x <- position_coords[[metrics_inside_placement]][1]
    ann_y <- position_coords[[metrics_inside_placement]][2]
  } else {
    stop("Invalid metrics_inside_placement. Choose from 'upperright', 'upperleft', 'lowerright', or 'lowerleft'.")
  }
  
  if (add_metrics & !is_grouped) {
    
    metrics_text <- agreement_metrics(data = data, 
                                      truth = {{truth}}, 
                                      estimate = {{estimate}}, 
                                      metrics = {{metrics}}, 
                                      label = TRUE) %>%
      dplyr::pull(label)
    
    if (metrics_position == "inside") {
      
      metrics_text <- stringr::str_replace_all(string = metrics_text, pattern = "; ", replacement = "<br>")
      
      ann <- ggplot2::annotate(geom = "richtext", 
                               x = ann_x, 
                               y = ann_y, 
                               label = metrics_text, 
                               hjust = ifelse(metrics_inside_placement %in% c("upperright", "lowerright"), 1, 0),
                               vjust = ifelse(metrics_inside_placement %in% c("upperleft", "upperright"), 1, 0),
                               fill = alpha(colour = "white", 0.50),
                               label.color = NA)
      
      p <- p + ann
    }
    
    if (metrics_position == "outside") {
      p <- p + ggplot2::labs(subtitle = metrics_text)
    }
  }
  
  if (add_metrics & is_grouped) {
    metrics_text <- agreement_metrics(data = data, 
                                      truth = {{truth}}, 
                                      estimate = {{estimate}}, 
                                      metrics = {{metrics}}, 
                                      label = TRUE)
    
    if (metrics_position == "inside") {
      metrics_text <- 
        metrics_text %>% 
        dplyr::mutate(label = stringr::str_replace_all(string = label, pattern = "; ", replacement = "<br>"))
      
      p <- p +
        ggtext::geom_richtext(
          data = metrics_text,
          aes(x = ann_x, y = ann_y, label = label),
          hjust = ifelse(metrics_inside_placement %in% c("upperright", "lowerright"), 1, 0),
          vjust = ifelse(metrics_inside_placement %in% c("upperleft", "upperright"), 1, 0),
          inherit.aes = FALSE, size = 3,
          fill = scales::alpha(colour = "white", 0.50),
          label.color = NA
        )
    }
    
    # if (metrics_position == "outside") {
    #   # Logic for grouped data with metrics outside remains unchanged
    # }
  }
  
  return(p)
}