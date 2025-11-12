#' Scatterplot with Truth and Estimate Values
#'
#' This function creates a scatterplot comparing `truth` and `estimate` values. It supports grouped data,
#' adding facets for each group, and can optionally include agreement metrics as text annotations in the plot.
#' Metrics can be positioned either inside the plot area or outside as subtitles or facet labels.
#'
#' The function can automatically switch from simple points (`geom_point()`) to density-colored points
#' (`ggpointdensity::geom_pointdensity()`) when large sample sizes are detected, helping to mitigate overplotting.
#'
#' @param data A data frame or tibble. Can be grouped (using `dplyr::group_by`) to create faceted plots.
#' @param truth The column name in `data` containing truth values. Should be unquoted.
#' @param estimate The column name in `data` containing estimate values. Should be unquoted.
#' @param metrics A list of metrics to compute and display. Metrics can include almost any function from the `yardstick` package
#'   (e.g., `rsq`, `rmse`, `mape`). Defaults to `list(rsq, md, rmd, rmse, rrmse)`. Set to `NULL` to disable.
#' @param metrics_position A character string indicating where to display metrics. Options are `"inside"`
#'   (as annotations within the plot) or `"outside"` (as subtitle or facet labels). Defaults to `"inside"`.
#' @param metrics_inside_placement A character string indicating the position of the metrics within the plot.
#'   Options are `"upperright"`, `"upperleft"`, `"lowerright"`, or `"lowerleft"`. Defaults to `"upperleft"`.
#' @param point_style Character; one of `c("point", "pointdensity", "auto")`.
#'   - `"point"` uses `geom_point()`.
#'   - `"pointdensity"` uses `ggpointdensity::geom_pointdensity()`, coloring points by density.
#'   - `"auto"` automatically switches to `"pointdensity"` when `nrow(data) >= density_switch_n`.
#' @param density_scale Character; one of `c("absolute", "relative")`. Controls how colors represent density:
#'   - `"absolute"` maps color to `after_stat(density)` with a global scale shared across facets, using a mild `"sqrt"` transform.
#'     This is suitable when comparing density magnitudes between facets.
#'   - `"relative"` maps color to `after_stat(ndensity)` (values normalized to [0,1] per facet). This emphasizes local patterns
#'     but is not directly comparable across facets.
#' @param density_adjust Numeric passed to `geom_pointdensity(adjust=)` (bandwidth multiplier). Ignored if `point_style="point"`.
#' @param density_method One of `c("auto", "kde2d", "neighbors")` for `geom_pointdensity(method=)`. Ignored if `point_style="point"`.
#' @param density_show_legend Logical; show a colorbar for density. Defaults to `FALSE`. If `TRUE`, the legend label reflects
#'   the selected `density_scale` (either "Point density" or "Relative density (per facet)"). Ignored if `point_style="point"`.
#' @param density_switch_n Integer threshold used when `point_style="auto"` (default 5000).
#' @param ... Additional parameters to control plot appearance and advanced color options:
#'   - `points_color`: Color of points (default `"black"`). Ignored for `pointdensity` when density is mapped.
#'   - `points_size`: Size of points (default `2`).
#'   - `points_shape`: Shape of points (default `1`).
#'   - `points_alpha`: Transparency of points (default `1`).
#'   - `text_size`: Text size (pt) for metrics (default `10`).
#'   - `text_background_alpha`: Transparency of metrics text background (default `0.5`; `0` disables background).
#'   - `metrics_nlines`: Split metrics text into multiple lines (default `1` line).
#'   - `density_palette`: Name of viridis palette to use for density mapping (`"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`).
#'   - `density_fixed_color`: Optional single color (e.g., `"darkred"`) to draw all points, disabling density coloring.
#'   - `density_scale_custom`: A custom ggplot2 scale (e.g., `scale_color_distiller(palette="Reds")`) to override the default viridis scale.
#'
#' @details
#' The function dynamically calculates axis ranges based on the `truth` and `estimate` values, ensuring a square plot using
#' `coord_fixed()`. For grouped data, it uses `facet_wrap()` to create separate scatterplots for each group.
#'
#' When `point_style = "pointdensity"`, points are colored by their local density to reduce overplotting.
#' The `density_scale` argument determines whether color is scaled globally (`"absolute"`) or normalized per facet (`"relative"`).
#' The color palette can be changed with `density_palette`, replaced with a fixed color using `density_fixed_color`,
#' or overridden entirely with a custom ggplot2 scale passed via `density_scale_custom`.
#'
#' Agreement metrics are calculated using the `agreement_metrics()` function and displayed according to `metrics_position`.
#' For grouped data with `metrics_position = "outside"`, metrics are added to the facet labels; with `"inside"`, they are displayed
#' as text annotations within each plot.
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
#' # ---------------------------------------------------------------------
#' # Point density coloring & controls
#' # ---------------------------------------------------------------------
#'
#' # 1) Force point-density with ABSOLUTE scale (comparable across facets)
#' scatter(df, truth, estimate,
#'         point_style = "pointdensity",
#'         density_scale = "absolute",
#'         density_show_legend = TRUE)
#'
#' # 2) Force point-density with RELATIVE scale (0–1 per facet); legend off
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate,
#'           point_style = "pointdensity",
#'           density_scale = "relative",
#'           density_show_legend = FALSE)
#'
#' # 3) Change the palette used for density mapping (viridis option)
#' scatter(df, truth, estimate,
#'         point_style = "pointdensity",
#'         density_scale = "absolute",
#'         density_show_legend = TRUE,
#'         density_palette = "plasma")
#'
#'
#' # 4) Provide a CUSTOM ggplot2 color scale (overrides viridis)
#' scatter(df, truth, estimate,
#'         point_style = "pointdensity",
#'         density_scale = "absolute",
#'         density_scale_custom = ggplot2::scale_color_distiller(palette = "Reds"),
#'         density_show_legend = TRUE)
#'
#' # 5) Auto-switch to point-density for larger datasets
#' # (uses 'density_switch_n' threshold; here we keep it small for example)
#' scatter(df, truth, estimate,
#'         point_style = "auto",
#'         density_switch_n = 100)  # switches to pointdensity at n >= 100
#'
#' # 6) Alternative density method & smoothing (neighbors + adjust)
#' scatter(df, truth, estimate,
#'         point_style = "pointdensity",
#'         density_scale = "absolute",
#'         density_method = "neighbors",
#'         density_adjust = 1.3,
#'         density_show_legend = TRUE)
#'
#' @export
scatter <- function(
  data,
  truth,
  estimate,
  metrics = list(rsq, md, rmd, rmse, rrmse),
  metrics_position = "inside",
  metrics_inside_placement = "upperleft",
  point_style = c("point", "pointdensity", "auto"),
  density_scale = c("absolute", "relative"),
  density_adjust = 1,
  density_method = c("auto", "kde2d", "neighbors"),
  density_show_legend = FALSE,
  density_switch_n = 5000,
  ...
) {
  point_style <- match.arg(point_style)
  density_scale <- match.arg(density_scale)
  density_method <- match.arg(density_method)

  # Ensure the truth and estimate columns exist
  if (
    !all(
      c(
        as.character(substitute(truth)),
        as.character(substitute(estimate))
      ) %in%
        colnames(data)
    )
  ) {
    stop("The specified truth and estimate variables do not exist in the data.")
  }

  # Capture additional parameters
  extra_params <- list(...)
  `%||%` <- function(x, y) if (is.null(x)) y else x

  points_color <- extra_params$points_color %||% "black"
  points_size <- extra_params$points_size %||% 2
  points_shape <- extra_params$points_shape %||% 1
  points_alpha <- extra_params$points_alpha %||% 1
  text_background_alpha <- extra_params$text_background_alpha %||% 0.5
  text_size <- extra_params$text_size %||% 10
  metrics_nlines <- extra_params$metrics_nlines %||% 1

  # NEW: density color controls via ...
  density_palette <- extra_params$density_palette %||% "viridis"
  density_fixed_color <- extra_params$density_fixed_color %||% NULL
  density_scale_custom <- extra_params$density_scale_custom %||% NULL

  # Check if any metrics provided
  add_metrics <- !is.null(metrics)

  # Check if the data is grouped
  is_grouped <- dplyr::is_grouped_df(data)

  # Combined range for square plot
  range_values <- range(
    data[[as.character(substitute(truth))]],
    data[[as.character(substitute(estimate))]],
    na.rm = TRUE
  )

  # choose style (if auto)
  if (point_style == "auto") {
    point_style <- if (nrow(data) >= density_switch_n) {
      "pointdensity"
    } else {
      "point"
    }
  }

  # Base plot + point layer
  p <-
    ggplot2::ggplot(data, ggplot2::aes(y = {{ truth }}, x = {{ estimate }})) +
    {
      if (point_style == "point") {
        ggplot2::geom_point(
          shape = points_shape,
          size = points_size,
          color = points_color,
          alpha = points_alpha
        )
      } else {
        # pointdensity path
        if (!is.null(density_fixed_color)) {
          # Fixed color requested: no density mapping, no legend
          ggpointdensity::geom_pointdensity(
            adjust = density_adjust,
            method = density_method,
            size = points_size,
            shape = points_shape,
            alpha = points_alpha,
            color = density_fixed_color,
            show.legend = FALSE
          )
        } else {
          # Map color to density or ndensity
          ggpointdensity::geom_pointdensity(
            ggplot2::aes(
              color = ggplot2::after_stat(
                if (density_scale == "relative") ndensity else density
              )
            ),
            adjust = density_adjust,
            method = density_method,
            size = points_size,
            shape = points_shape,
            alpha = points_alpha,
            show.legend = density_show_legend
          )
        }
      }
    } +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "grey50") +
    ggplot2::labs(
      y = as.character(substitute(truth)),
      x = as.character(substitute(estimate))
    ) +
    ggplot2::coord_fixed(xlim = range_values, ylim = range_values) +
    theme_baseR()

  # Density color scale (only when using pointdensity AND mapping color)
  if (point_style == "pointdensity" && is.null(density_fixed_color)) {
    if (!is.null(density_scale_custom)) {
      # User supplies a full custom ggplot2 scale
      p <- p + density_scale_custom
    } else {
      # Built-in viridis (option from density_palette)
      p <- p +
        ggplot2::scale_color_viridis_c(
          option = match.arg(
            density_palette,
            c("viridis", "magma", "plasma", "inferno", "cividis")
          ),
          name = if (density_scale == "relative") {
            "Relative density\n(per facet)"
          } else {
            "Point density"
          },
          guide = if (density_show_legend) "colourbar" else "none",
          trans = if (density_scale == "absolute") "sqrt" else "identity"
        )
    }
  }

  # Faceting if grouped
  if (is_grouped) {
    facet_vars <- dplyr::group_vars(data)
    groups_count <- length(facet_vars)
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(!!!rlang::syms(facet_vars)),
        scales = "fixed"
      )
  } else {
    groups_count <- 0L
  }

  # Annotation anchor based on placement
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
    stop(
      "Invalid metrics_inside_placement. Choose from 'upperright', 'upperleft', 'lowerright', or 'lowerleft'."
    )
  }

  # ---------- Metrics ----------
  if (add_metrics & !is_grouped) {
    metrics_text <- agreement_metrics(
      data = data,
      truth = {{ truth }},
      estimate = {{ estimate }},
      metrics = {{ metrics }},
      label = TRUE
    ) %>%
      dplyr::pull(label)

    if (metrics_position == "inside") {
      metrics_text <- stringr::str_replace_all(metrics_text, "; ", "<br>")

      ann <- ggplot2::annotate(
        geom = "richtext",
        x = ann_x,
        y = ann_y,
        label = metrics_text,
        size = text_size / 2.845276, # pt -> mm
        hjust = ifelse(
          metrics_inside_placement %in% c("upperright", "lowerright"),
          1,
          0
        ),
        vjust = ifelse(
          metrics_inside_placement %in% c("upperleft", "upperright"),
          1,
          0
        ),
        fill = scales::alpha(colour = "white", alpha = text_background_alpha),
        label.color = NA
      )

      p <- p + ann
    }

    if (metrics_position == "outside") {
      if (metrics_nlines > 1) {
        parts <- stringr::str_split(metrics_text, ";\\s*")[[1]]
        n_per_line <- ceiling(length(parts) / metrics_nlines)
        grouped <- split(parts, ceiling(seq_along(parts) / n_per_line))
        metrics_text <- paste(
          sapply(grouped, paste, collapse = "; "),
          collapse = "\n"
        )
      }
      p <- p + ggplot2::labs(subtitle = metrics_text)
    }
  }

  if (add_metrics & is_grouped) {
    metrics_text <- agreement_metrics(
      data = data,
      truth = {{ truth }},
      estimate = {{ estimate }},
      metrics = {{ metrics }},
      label = TRUE
    )

    if (metrics_position == "inside") {
      metrics_text <- metrics_text %>%
        dplyr::mutate(label = stringr::str_replace_all(label, "; ", "<br>"))

      p <- p +
        ggtext::geom_richtext(
          data = metrics_text,
          ggplot2::aes(x = ann_x, y = ann_y, label = label),
          hjust = ifelse(
            metrics_inside_placement %in% c("upperright", "lowerright"),
            1,
            0
          ),
          vjust = ifelse(
            metrics_inside_placement %in% c("upperleft", "upperright"),
            1,
            0
          ),
          inherit.aes = FALSE,
          size = text_size / 2.845276,
          fill = scales::alpha(colour = "white", alpha = text_background_alpha),
          label.color = NA
        )
    }

    if (metrics_position == "outside") {
      if (groups_count == 1) {
        gv <- dplyr::group_vars(data)

        metrics_text <- metrics_text %>%
          dplyr::mutate(
            label = {
              label_parts <- stringr::str_split(label, ";\\s*")
              label_wrapped <- purrr::map_chr(label_parts, function(parts) {
                if (metrics_nlines > 1) {
                  n_per_line <- ceiling(length(parts) / metrics_nlines)
                  grouped <- split(
                    parts,
                    ceiling(seq_along(parts) / n_per_line)
                  )
                  paste(
                    sapply(grouped, paste, collapse = "; "),
                    collapse = "<br>"
                  )
                } else {
                  paste(parts, collapse = "; ")
                }
              })
              paste0(!!rlang::sym(gv), "<br>", label_wrapped)
            }
          )

        custom_labeller <- ggplot2::as_labeller(stats::setNames(
          metrics_text$label,
          metrics_text[[gv]]
        ))

        p <- p +
          ggplot2::facet_wrap(
            ggplot2::vars(!!!rlang::syms(gv)),
            scales = "fixed",
            labeller = custom_labeller
          ) +
          ggplot2::theme(
            strip.text = ggtext::element_textbox(halign = 0.5, size = text_size)
          )
      } else {
        metrics_text <- metrics_text %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            group_label = paste(
              dplyr::across(dplyr::all_of(dplyr::group_vars(data))),
              collapse = " | "
            ),
            label = {
              parts <- stringr::str_split(label, ";\\s*")[[1]]
              if (metrics_nlines > 1) {
                n_per_line <- ceiling(length(parts) / metrics_nlines)
                grouped <- split(parts, ceiling(seq_along(parts) / n_per_line))
                wrapped <- paste(
                  sapply(grouped, paste, collapse = "; "),
                  collapse = "<br>"
                )
              } else {
                wrapped <- paste(parts, collapse = "; ")
              }
              paste0(group_label, "<br>", wrapped)
            }
          ) %>%
          dplyr::ungroup()

        data <- data %>%
          dplyr::mutate(
            group_label = paste(
              !!!rlang::syms(dplyr::group_vars(data)),
              sep = " | "
            )
          )

        custom_labeller <- ggplot2::as_labeller(stats::setNames(
          metrics_text$label,
          metrics_text$group_label
        ))

        p <- p +
          ggplot2::facet_wrap(
            ~group_label,
            scales = "fixed",
            labeller = custom_labeller
          ) +
          ggplot2::theme(
            strip.text = ggtext::element_textbox(halign = 0.5, size = text_size)
          )
      }
    }
  }

  return(p)
}
