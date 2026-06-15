#' Metric Display Helpers
#'
#' @description
#' Helpers for controlling how metrics are displayed in labels created by
#' `agreement_metrics()` and `scatter()`. The underlying metrics are still
#' calculated by `yardstick`; these helpers only affect label formatting.
#'
#' @param metric A yardstick-style metric function.
#' @param value A yardstick-style metric function for the primary value.
#' @param percent A yardstick-style metric function for the relative or percent value.
#' @param format A format string using `{value}` for single metrics, or `{value}`
#' and `{percent}` for paired metrics. Numeric format specifiers such as
#' `{value:.1f}` and `{percent:.0f}` are supported.
#'
#' @return A metric display specification for use in the `metrics` argument.
#'
#' @examples
#' metrics <- list(
#'   "RMSE" = metric_pair(yardstick::rmse, rrmse, "{value:.1f} ({percent:.0f}%)"),
#'   "bias" = metric_pair(md, rmd, "{value:.1f} ({percent:.1f}%)"),
#'   "R2" = metric_format(yardstick::rsq, "{value:.2f}")
#' )
#'
#' @export
metric_pair <- function(value, percent, format = "{value} ({percent}%)") {
  if (!is.function(value)) {
    stop("`value` must be a metric function.", call. = FALSE)
  }
  if (!is.function(percent)) {
    stop("`percent` must be a metric function.", call. = FALSE)
  }
  if (!is.character(format) || length(format) != 1) {
    stop("`format` must be a single character string.", call. = FALSE)
  }

  structure(
    list(value = value, percent = percent, format = format),
    class = "scatter_metric_pair"
  )
}

#' @rdname metric_pair
#' @export
metric_format <- function(metric, format = "{value}") {
  if (!is.function(metric)) {
    stop("`metric` must be a metric function.", call. = FALSE)
  }
  if (!is.character(format) || length(format) != 1) {
    stop("`format` must be a single character string.", call. = FALSE)
  }

  structure(
    list(metric = metric, format = format),
    class = "scatter_metric_format"
  )
}

prepare_metric_display <- function(metrics) {
  if (is.null(names(metrics))) {
    metric_names <- rep("", length(metrics))
  } else {
    metric_names <- names(metrics)
  }

  expanded_metrics <- list()
  custom_metric_names <- character()
  label_specs <- list()

  for (i in seq_along(metrics)) {
    metric <- metrics[[i]]
    metric_name <- metric_names[[i]]

    if (inherits(metric, "scatter_metric_pair")) {
      if (identical(metric_name, "")) {
        stop("Entries created by `metric_pair()` must be named.", call. = FALSE)
      }

      value_index <- length(expanded_metrics) + 1L
      expanded_metrics[[value_index]] <- metric$value
      custom_metric_names[[value_index]] <- metric_name

      percent_index <- length(expanded_metrics) + 1L
      expanded_metrics[[percent_index]] <- metric$percent
      custom_metric_names[[percent_index]] <- paste0(metric_name, "%")

      label_specs[[length(label_specs) + 1L]] <- list(
        type = "pair",
        label = metric_name,
        indices = c(value_index, percent_index),
        format = metric$format
      )
    } else if (inherits(metric, "scatter_metric_format")) {
      metric_index <- length(expanded_metrics) + 1L
      expanded_metrics[[metric_index]] <- metric$metric
      custom_metric_names[[metric_index]] <- metric_name

      label_specs[[length(label_specs) + 1L]] <- list(
        type = "single",
        label = metric_name,
        indices = metric_index,
        format = metric$format
      )
    } else {
      metric_index <- length(expanded_metrics) + 1L
      expanded_metrics[[metric_index]] <- metric
      custom_metric_names[[metric_index]] <- metric_name

      label_specs[[length(label_specs) + 1L]] <- list(
        type = "single",
        label = metric_name,
        indices = metric_index,
        format = "{value}"
      )
    }
  }

  list(
    metrics = expanded_metrics,
    names = custom_metric_names,
    label_specs = label_specs
  )
}

finalize_metric_display_specs <- function(label_specs, metric_names) {
  lapply(label_specs, function(spec) {
    spec$columns <- metric_names[spec$indices]
    if (identical(spec$label, "")) {
      spec$label <- spec$columns[[1]]
    }
    spec
  })
}

build_metric_labels <- function(data, label_specs) {
  vapply(seq_len(nrow(data)), function(i) {
    pieces <- vapply(label_specs, function(spec) {
      if (identical(spec$type, "pair")) {
        value <- data[[spec$columns[[1]]]][[i]]
        percent <- data[[spec$columns[[2]]]][[i]]
        rendered <- render_metric_format(
          spec$format,
          value = value,
          percent = percent
        )
      } else {
        value <- data[[spec$columns[[1]]]][[i]]
        rendered <- render_metric_format(spec$format, value = value)
      }

      paste(spec$label, rendered, sep = ": ")
    }, FUN.VALUE = character(1))

    paste(pieces, collapse = "; ")
  }, FUN.VALUE = character(1))
}

render_metric_format <- function(format, value, percent = NULL) {
  matches <- gregexpr("\\{(value|percent)(:[^}]*)?\\}", format, perl = TRUE)
  tokens <- regmatches(format, matches)[[1]]

  if (length(tokens) == 0 || identical(tokens, character(0))) {
    return(format)
  }

  out <- format

  for (token in unique(tokens)) {
    parsed <- regmatches(
      token,
      regexec("^\\{(value|percent)(?::([^}]+))?\\}$", token, perl = TRUE)
    )[[1]]

    key <- parsed[[2]]
    spec <- if (length(parsed) >= 3) parsed[[3]] else ""
    replacement <- switch(
      key,
      value = format_metric_number(value, spec),
      percent = format_metric_number(percent, spec)
    )

    out <- gsub(token, replacement, out, fixed = TRUE)
  }

  out
}

format_metric_number <- function(x, spec = "") {
  if (is.null(x) || is.na(x)) {
    return("NA")
  }

  precision <- regmatches(spec, regexec("^\\.([0-9]+)f$", spec))[[1]]

  if (length(precision) == 2) {
    return(sprintf(paste0("%.", precision[[2]], "f"), x))
  }

  as.character(x)
}
