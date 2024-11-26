#' Calculate Agreement Metrics for Observed and Predicted Values
#'
#' Computes various metrics (e.g., R2, bias, RMSE) to evaluate the agreement
#' between observed and predicted values. Supports grouped data and includes
#' an option to add overall metrics for the entire dataset.
#'
#' @param data A data frame or tibble containing the observed and predicted values, as well as any grouping variables.
#' @param observed The name of the column containing observed values. Must be unquoted (e.g., `observed`).
#' @param predicted The name of the column containing predicted values. Must be unquoted (e.g., `predicted`).
#' @param add_overall Logical, default `TRUE`. If `TRUE`, an additional row is added with overall metrics calculated for the entire dataset.
#' @param r2_method A character string indicating the method for calculating \(R^2\). 
#'   Options are `"sum_of_squares"` (default) for the traditional \(R^2\) based on 
#'   residual and total sums of squares, or `"correlation"` for the square of the 
#'   Pearson correlation coefficient between observed and predicted values.
#' @return A tibble containing the calculated metrics:
#'   \describe{
#'     \item{Grouping Variables}{Grouping variables from the input data, if present.}
#'     \item{n}{Number of valid (non-missing) observations used for calculations.}
#'     \item{R2}{Coefficient of determination (R-squared).}
#'     \item{bias}{Mean difference between predicted and observed values.}
#'     \item{bias_perc}{Relative bias, expressed as a percentage.}
#'     \item{RMSE}{Root Mean Squared Error.}
#'     \item{RMSE_perc}{Relative RMSE, expressed as a percentage.}
#'   }
#'
#' @details
#' If the input data is grouped using `group_by()`, metrics are calculated separately for each group.
#' When `add_overall = TRUE`, a row with overall metrics (calculated on the entire dataset) is added to the result.
#' This function supports grouping variables of any type but ensures that grouping variables are consistently of type `character` in the output.
#'
#' @examples
#' library(dplyr)
#' 
#' # Example data
#' data <- tibble(
#'   species = c("A", "A", "B", "B", "C", "C"),
#'   region = c("North", "North", "South", "South", "East", "East"),
#'   observed = c(10, 15, 20, NA, 25, 30),
#'   predicted = c(12, 14, 22, NA, 23, 28)
#' )
#' 
#' # Grouped calculation with overall statistics
#' data %>%
#'   group_by(species, region) %>%
#'   agreement_metrics(observed = observed, predicted = predicted)
#'
#' # Ungrouped calculation
#' agreement_metrics(data, observed = observed, predicted = predicted)
#'
#' @export



agreement_metrics <- function(data, observed, predicted, add_overall = TRUE, r2_method = c("sum_of_squares", "correlation")) {
  # Ensure the input is a data frame or tibble
  if (!inherits(data, "data.frame")) {
    stop("The input must be a data frame or tibble.")
  }
  
  # Match the method argument
  r2_method <- match.arg(r2_method)
  
  # Convert column names to symbols
  observed <- rlang::ensym(observed)
  predicted <- rlang::ensym(predicted)
  
  # Define the core calculation logic
  calc <- function(data) {
    observed <- data %>% pull(!!observed)
    predicted <- data %>% pull(!!predicted)
    
    # Check for NA values
    na_count <- sum(is.na(observed) | is.na(predicted))
    if (na_count > 0) {
      warning(paste(na_count, "NA value(s) were removed from the input columns in a group."))
    }
    
    # Remove missing values
    valid <- !is.na(observed) & !is.na(predicted)
    observed <- observed[valid]
    predicted <- predicted[valid]
    
    # Metrics calculations
    residuals <- observed - predicted
    
    
    # Calculate R2 based on selected method
    if (r2_method == "sum_of_squares") {
      ss_total <- sum((observed - mean(observed))^2)
      ss_residual <- sum(residuals^2)
      r2 <- 1 - (ss_residual / ss_total)
    } else if (r2_method == "correlation") {
      r2 <- cor(observed, predicted)^2
    }
    
    bias <- mean(predicted - observed)
    relative_bias <- (bias / mean(observed)) * 100
    rmse <- sqrt(mean(residuals^2))
    relative_rmse <- (rmse / mean(observed)) * 100
    
    tibble(
      n = length(residuals),
      R2 = r2,
      bias = bias,
      bias_perc = relative_bias,
      RMSE = rmse,
      RMSE_perc = relative_rmse
    )
  }
  
  if (dplyr::is_grouped_df(data)) {
    # Calculate metrics for each group
    grouped_metrics <- data %>%
      group_split() %>%
      purrr::map_dfr(~ calc(.x) %>% mutate(across(everything(), as.numeric))) %>%
      bind_cols(data %>% group_keys())
    
    if (add_overall) {
      # Get grouping variables safely
      grouping_columns <- group_vars(data)
      
      # Calculate metrics for the entire dataset (ungrouped)
      overall_metrics <- calc(data) %>%
        mutate(across(everything(), as.numeric)) %>%
        bind_cols(
          tibble(!!!setNames(rep(list("Overall"), length(grouping_columns)), grouping_columns)) %>%
            mutate(across(all_of(grouping_columns), as.character))
        )
      
      # Ensure all grouping columns are character
      grouped_metrics <- grouped_metrics %>%
        mutate(across(all_of(grouping_columns), as.character))
      overall_metrics <- overall_metrics %>%
        mutate(across(all_of(grouping_columns), as.character))
      
      # Combine grouped and overall metrics
      combined <- bind_rows(grouped_metrics, overall_metrics)
    } else {
      combined <- grouped_metrics
    }
  } else {
    # Calculate metrics for ungrouped data
    combined <- calc(data)
  }
  
  # Reorder columns to ensure grouping columns are first
  if (dplyr::is_grouped_df(data)) {
    grouping_columns <- group_vars(data)
    combined <- combined %>%
      relocate(all_of(grouping_columns), .before = everything())
  }
  
  combined
}
