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
#'   (e.g., `rsq`, `rmse`, `mape`). Defaults to `list(rsq, md, rmd, rmse, rrmse)`. Set to `NULL` to disable.
#' @param metrics_position A character string indicating where to display metrics. Options are `"inside"` 
#'   (as annotations within the plot) or `"outside"` (as subtitle or facet labels). Defaults to `"inside"`.
#' @param metrics_inside_placement A character string indicating the position of the metrics within the plot. 
#'   Options are "upperright", "upperleft", "lowerright", or "lowerleft". Defaults to "upperright".
#' @param ... Additional parameters to control point aesthetics, including:
#'   - `points_color`: Color of points (default is "black").
#'   - `points_size`: Size of points (default is 2).
#'   - `points_shape`: Shape of points (default is 1).
#'   - `points_alpha`: Transparency of points (default is 1).
#'   - `text_size`: text size for the displayed metrics (default is 10 pt)
#'   - `text_background_alpha`: change the transparency of the metrics text background (default is 0.5). Disable with 0 (fully transparent)
#'   - `metrics_nlines`: allow to split the metrics text into n lines (default is 1 line)
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
                    metrics = list(rsq, md, rmd, rmse, rrmse),
                    metrics_position = "inside",
                    metrics_inside_placement = "upperleft",
                    ...) {
  
  # Ensure the truth and estimate columns exist
  if (!all(c(as.character(substitute(truth)), as.character(substitute(estimate))) %in% colnames(data))) {
    stop("The specified truth and estimate variables do not exist in the data.")
  }
  
  # Capture additional parameters
  extra_params <- list(...)
  points_color <- extra_params$points_color %||% "black"
  points_size <- extra_params$points_size %||% 2
  points_shape <- extra_params$points_shape %||% 1
  points_alpha <- extra_params$points_alpha %||% 1
  text_background_alpha <- extra_params$text_background_alpha %||% 0.5
  text_size <- extra_params$text_size %||% 10
  
  metrics_nlines <- extra_params$metrics_nlines %||% 1
  
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
    ggplot2::geom_point(shape = points_shape, size = points_size, color = points_color, alpha = points_alpha) +  
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
                               size = text_size / 2.845276,
                               hjust = ifelse(metrics_inside_placement %in% c("upperright", "lowerright"), 1, 0),
                               vjust = ifelse(metrics_inside_placement %in% c("upperleft", "upperright"), 1, 0),
                               fill = alpha(colour = "white", text_background_alpha),
                               label.color = NA)
      
      p <- p + ann
    }
    
    # if (metrics_position == "outside") {
    #   p <- p + ggplot2::labs(subtitle = metrics_text)
    # }
    
    if (metrics_position == "outside") {
      # Split and wrap metrics_text if needed
      if (metrics_nlines > 1) {
        parts <- stringr::str_split(metrics_text, ";\\s*")[[1]]
        n_per_line <- ceiling(length(parts) / metrics_nlines)
        grouped <- split(parts, ceiling(seq_along(parts) / n_per_line))
        wrapped <- paste(sapply(grouped, paste, collapse = "; "), collapse = "\n")
        metrics_text <- wrapped
      }
      
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
          inherit.aes = FALSE, 
          size = text_size / 2.845276, #converting to pt
          fill = scales::alpha(colour = "white", text_background_alpha),
          label.color = NA
        )
    }
    
    
    if(metrics_position=="outside") {
      
      if(groups_count == 1) {
        
        #modify the labels 
        # metrics_text <- 
        #   metrics_text %>%
        #   dplyr::mutate(label = paste0(
        #     (!!!rlang::syms(facet_vars)),"<br>", 
        #     label # text size could be adjusted here
        #   ))
        
        metrics_text <- 
          metrics_text %>%
          dplyr::mutate(
            label = {
              label_parts <- stringr::str_split(label, ";\\s*")
              label_wrapped <- purrr::map_chr(label_parts, function(parts) {
                if (metrics_nlines > 1) {
                  n_per_line <- ceiling(length(parts) / metrics_nlines)
                  grouped <- split(parts, ceiling(seq_along(parts) / n_per_line))
                  paste(sapply(grouped, paste, collapse = "; "), collapse = "<br>")
                } else {
                  paste(parts, collapse = "; ")
                }
              })
              paste0(!!rlang::sym(facet_vars), "<br>", label_wrapped)
            }
          )
        
        
        custom_labeller <- ggplot2::as_labeller(setNames(metrics_text$label, metrics_text[[facet_vars]]))
        
        p <- p + 
          ggplot2::facet_wrap(
            ggplot2::vars(!!!rlang::syms(facet_vars)),
            scales = "fixed",
            labeller = custom_labeller
          ) + ggplot2::theme(
            strip.text = ggtext::element_textbox(halign = 0.5, size = text_size)
          )
      } 
      
      if (groups_count > 1) {
        
        # if more than one grouping variable then they need to be combined to make labeller function to work
        # metrics_text <- metrics_text %>%
        #   dplyr::rowwise() %>%
        #   dplyr::mutate(
        #     group_label = paste(dplyr::across(dplyr::all_of(facet_vars)), collapse = " | "),
        #     label = paste0(group_label, "<br>", label)
        #   ) %>%
        #   dplyr::ungroup()
        
        metrics_text <- metrics_text %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            group_label = paste(dplyr::across(dplyr::all_of(facet_vars)), collapse = " | "),
            label = {
              parts <- stringr::str_split(label, ";\\s*")[[1]]
              if (metrics_nlines > 1) {
                n_per_line <- ceiling(length(parts) / metrics_nlines)
                grouped <- split(parts, ceiling(seq_along(parts) / n_per_line))
                wrapped <- paste(sapply(grouped, paste, collapse = "; "), collapse = "<br>")
              } else {
                wrapped <- paste(parts, collapse = "; ")
              }
              paste0(group_label, "<br>", wrapped)
            }
          ) %>%
          dplyr::ungroup()
        
        
        
        data <- data %>% 
          dplyr::mutate(
            group_label = paste(!!!rlang::syms(facet_vars), sep = " | ")
          )
        
        custom_labeller <- ggplot2::as_labeller(setNames(metrics_text$label, 
                                                         metrics_text$group_label))
        # default=label_wrap_gen())#doesn't work
        
        p <- p + 
          ggplot2::facet_wrap(
            ~group_label,
            scales = "fixed",
            labeller = custom_labeller
          ) + 
          ggplot2::theme(
            strip.text = ggtext::element_textbox(halign = 0.5, size=text_size)
          )
        
      }
    }  
    
    
  }
  
  return(p)
}