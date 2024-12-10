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
#'   (e.g., `rsq`, `rmse`, `mape`). Defaults to `list(rsq, msd, rmse, mape)`. Set to `NULL` to disable.
#' @param metrics_position A character string indicating where to display metrics. Options are `"inside"` 
#'   (as annotations within the plot) or `"outside"` (as subtitles or facet labels). Defaults to `"inside"`.
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
#' scatter(df, truth, estimate, metrics = list(rsq, mape), metrics_position = "inside")
#'
#' # Scatterplot with agreement metrics (outside plot as subtitle)
#' scatter(df, truth, estimate, metrics = list(rsq, rmse), metrics_position = "outside")
#'
#' # Grouped scatterplot with agreement metrics inside
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate, metrics = list(rsq, mape), metrics_position = "inside")
#'
#' # Grouped scatterplot with agreement metrics outside as facet labels
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate, metrics = list(rsq, rmse), metrics_position = "outside")
#'
#' @export
scatter <- function(data, truth, estimate, 
                     metrics = list(yardstick::rsq,
                                    yardstick::msd,
                                    yardstick::rmse,
                                    yardstick::mape),
                     metrics_position="inside"
                     
) {
  
  # Ensure the truth and estimate columns exist
  if (!all(c(as.character(substitute(truth)), as.character(substitute(estimate))) %in% colnames(data))) {
    stop("The specified truth and estimate variables do not exist in the data.")
  }
  
  #check if any metrics provided
  add_metrics <- ifelse(!is.null(metrics), TRUE, FALSE)
  
  
  # Check if the data is grouped
  is_grouped <- dplyr::is_grouped_df(data)
  
  # Calculate the combined range of truth and estimate values
  range_values <- range(data[[as.character(substitute(truth))]],
                        data[[as.character(substitute(estimate))]],
                        na.rm = TRUE)
  
  # Start building the plot
  p <- ggplot(data, aes(y = {{truth}}, x = {{estimate}})) +
    geom_point(shape = 1, size = 2) +  
    geom_abline(intercept = 0, slope = 1, color = "grey50") +  # Add 1:1 line
    labs(
      y = as.character(substitute(truth)),
      x = as.character(substitute(estimate))
    ) +
    coord_fixed(xlim = range_values, ylim = range_values) +  # Ensure equal axis limits and square plot
    theme_baseR()  # Apply custom theme
  
  # Add faceting if the data is grouped
  if (is_grouped) {
    facet_vars <- dplyr::group_vars(data)
    
    #how many grouping vars
    groups_count <- length(facet_vars)
    
    p <- p + facet_wrap(vars(!!!rlang::syms(facet_vars)), scales = "fixed")
  }
  
  if(add_metrics & !is_grouped) {
    
    #calculate metrics for the ungrouped scenario
    metrics_text <- agreement_metrics(data = data, 
                                 truth = {{truth}}, 
                                 estimate = {{estimate}}, 
                                 metrics={{metrics}}, 
                                 label=TRUE ) %>%
      pull(label)
    
    #if display is as subtitle the leave as is
    # if display is inside the plot, then change ; to <br>
    if(metrics_position=="inside") {
      
      metrics_text <- str_replace_all(string = metrics_text , pattern = "; ", replacement = "<br>")
      
      ann_x <- min(range_values)
      ann_y <- max(range_values)
      ann_hjust <- 0
      ann_vjust <- 0.9
      
      ann <- ggplot2::annotate(geom = "richtext", 
                               x = ann_x, 
                               y = ann_y, 
                               label = metrics_text, 
                               hjust = ann_hjust, 
                               vjust = ann_vjust,
                               fill = alpha(colour = "white", 0.50),
                               label.color = NA)
      
      # Add the textbox to the plot
      p <- p + ann}
    
    # if metrics outside then use subtitle
    if(metrics_position=="outside") {
      p <- p + labs(subtitle = metrics_text)
    }
  }
  
  if(add_metrics & is_grouped) {
    
    #calculate metrics for the grouped scenario
    metrics_text <- agreement_metrics(data = data, 
                                 truth = {{truth}}, 
                                 estimate = {{estimate}}, 
                                 metrics={{metrics}}, 
                                 label=TRUE ) 
    
    
    if(metrics_position=="inside") {
      
      metrics_text <- 
        metrics_text %>% mutate(label = str_replace_all(string = label , pattern = "; ", replacement = "<br>"))
      
      p <-p +
        geom_richtext(
          data = metrics_text,
          aes(x = min(range_values), y = max(range_values), label = label),
          hjust = 0, vjust = 1, inherit.aes = FALSE, size = 3,
          fill = alpha(colour = "white", 0.50),
          label.color = NA
        )
      
    }
    
    if(metrics_position=="outside") {
      
      if(groups_count == 1) {
        
        #modify the labels 
        metrics_text <- 
          metrics_text %>%
          mutate(label = paste0(
            (!!!rlang::syms(facet_vars)),"<br>", 
            label # text size could be adjusted here
          ))
        
        
        custom_labeller <- as_labeller(setNames(metrics_text$label, metrics_text[[facet_vars]]))
        
        p <- p + facet_wrap(
          vars(!!!rlang::syms(facet_vars)),
          scales = "fixed",
          labeller = custom_labeller
        ) + theme(
          strip.text = element_textbox(halign = 0.5)
        )
      } 
      
      if (groups_count > 1) {

        # if more than one grouping variable then they need to be combined to make labeller function to work
        metrics_text <- metrics_text %>%
          rowwise() %>%
          mutate(
            group_label = paste(across(all_of(facet_vars)), collapse = " | "),
            label = paste0(group_label, "<br>", label)
          ) %>%
          ungroup()
        
        
        
        data <- data %>% 
          mutate(
            group_label = paste(!!!syms(facet_vars), sep = " | ")
          )
        
        custom_labeller <- as_labeller(setNames(metrics_text$label, 
                                                metrics_text$group_label))
                                       # default=label_wrap_gen())#doesn't work
        
        p <- p + facet_wrap(
          ~group_label,
          scales = "fixed",
          labeller = custom_labeller
        ) + theme(
          strip.text = element_textbox(halign = 0.5)
        )
        
      }
      
    }
    
  }
  
  return(p)
}

