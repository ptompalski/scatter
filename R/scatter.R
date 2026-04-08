#' Scatterplot with Truth and Estimate Values
#'
#' This function creates a scatterplot comparing \code{truth} (typically observed)
#' and \code{estimate} (typically predicted) values. By default, \code{truth} is mapped
#' to the x-axis and \code{estimate} to the y-axis, but this can be reversed using
#' the \code{swap_axes} argument. It supports grouped data,
#' adding facets for each group, and can optionally include agreement metrics as text annotations in the plot.
#' Metrics can be positioned either inside the plot area or outside as subtitles or facet labels.
#' The function can automatically switch from simple points (\code{geom_point()}) to density-colored points
#' (\code{ggpointdensity::geom_pointdensity()}) when large sample sizes are detected, helping to mitigate overplotting.
#'
#' @param data A data frame or tibble. Can be grouped (using \code{dplyr::group_by}) to create faceted plots.
#' @param truth The column name in \code{data} containing truth values. Should be unquoted.
#' @param estimate The column name in \code{data} containing estimate values. Should be unquoted.
#' @param metrics A list of metrics to compute and display. Metrics can include almost any function from the \pkg{yardstick} package
#'   (e.g., \code{rsq}, \code{rmse}, \code{mape}). This can be either an unnamed list of functions or a named list such as
#'   \code{list("R^2" = rsq, "bias\%" = rmd)}, in which case the provided names are used in the labels. Defaults to
#'   \code{list("R²" = rsq, "bias" = md, "bias\%" = rmd, "RMSE" = rmse, "RMSE\%" = rrmse)}. Set to \code{NULL} to disable.
#' @param metrics_position A character string indicating where to display metrics. Options are \code{"inside"}
#'   (as annotations within the plot) or \code{"outside"} (as subtitle or facet labels). Defaults to \code{"inside"}.
#' @param metrics_inside_placement A character string indicating the position of the metrics within the plot.
#'   Options are \code{"upperright"}, \code{"upperleft"}, \code{"lowerright"}, or \code{"lowerleft"}. Defaults to \code{"upperleft"}.
#' @param point_style Character; one of \code{c("point", "pointdensity", "auto")}.
#'   - \code{"point"} uses \code{geom_point()}.
#'   - \code{"pointdensity"} uses \code{ggpointdensity::geom_pointdensity()}, coloring points by density.
#'   - \code{"auto"} automatically switches to \code{"pointdensity"} when \code{nrow(data) >= density_switch_n}.
#' @param density_scale Character; one of \code{c("absolute", "relative")}. Controls how colors represent density:
#'   - \code{"absolute"} maps color to \code{after_stat(density)} with a global scale shared across facets, using a mild \code{"sqrt"} transform.
#'     This is suitable when comparing density magnitudes between facets.
#'   - \code{"relative"} maps color to \code{after_stat(ndensity)} (values normalized to [0,1] per facet). This emphasizes local patterns
#'     but is not directly comparable across facets.
#' @param swap_axes Logical; if \code{FALSE} (default), \code{truth} is mapped to the
#'   x-axis and \code{estimate} to the y-axis. If \code{TRUE}, the axes are swapped, with
#'   \code{estimate} on the x-axis and \code{truth} on the y-axis (i.e., the previous
#'   behavior of the function). This option affects only the visual orientation of the plot
#'   and does  affect how agreement metrics are calculated — metrics are
#'   always computed as \code{metric(truth, estimate)} regardless of axis order.
#' @param ... Additional parameters controlling plot appearance and advanced color options:
#' \describe{
#'   \item{\code{points_color}}{Color of points (default \code{"black"}). Ignored for pointdensity when density is mapped.}
#'   \item{\code{points_size}}{Size of points (default \code{2}).}
#'   \item{\code{points_shape}}{Shape of points (default \code{1}).}
#'   \item{\code{points_alpha}}{Transparency of points (default \code{1}).}
#'   \item{\code{density_adjust}}{Numeric passed to \code{geom_pointdensity(adjust = )} as the bandwidth multiplier. Ignored if \code{point_style = "point"}.}
#'   \item{\code{density_method}}{One of \code{c("auto", "kde2d", "neighbors")} for \code{geom_pointdensity(method = )}. Ignored if \code{point_style = "point"}.}
#'   \item{\code{density_show_legend}}{Logical; show a colorbar for density. Defaults to \code{FALSE}.}
#'   \item{\code{density_switch_n}}{Integer threshold used when \code{point_style = "auto"} (default \code{5000}).}
#'   \item{\code{facet_scale}}{One of \code{c("fixed", "free")}. Controls whether facets share the same square range or scale independently.}
#'   \item{\code{plot_range}}{Optional numeric vector of length 2 giving the visible axis range to use for both x and y axes. This keeps the plotting window square while allowing agreement metrics to still be calculated from all data.}
#'   \item{\code{text_size}}{Text size (pt) for metrics (default \code{10}).}
#'   \item{\code{text_background_alpha}}{Transparency of metrics text background (default \code{0.5}; \code{0} disables background).}
#'   \item{\code{metrics_nlines}}{Split metrics text into multiple lines (default \code{1} line).}
#'   \item{\code{density_palette}}{Name of viridis palette to use for density mapping: \code{"viridis"}, \code{"magma"}, \code{"plasma"}, \code{"inferno"}, or \code{"cividis"}.}
#'   \item{\code{density_fixed_color}}{Optional single color (for example \code{"darkred"}) to draw all points, disabling density coloring.}
#'   \item{\code{density_scale_custom}}{A custom ggplot2 scale (for example \code{scale_color_distiller(palette = "Reds")}) to override the default viridis scale.}
#' }
#'
#' @details
#' The function dynamically calculates axis ranges based on the \code{truth} and \code{estimate} values, ensuring a square plot using
#' \code{coord_fixed()}. Supplying \code{plot_range} overrides the visible range for both axes while preserving full-data metric
#' calculations. For grouped data, it uses \code{facet_wrap()} to create separate scatterplots for each group.
#'
#' When \code{point_style = "pointdensity"}, points are colored by their local density to reduce overplotting.
#' The \code{density_scale} argument determines whether color is scaled globally (\code{"absolute"}) or normalized per facet (\code{"relative"}).
#' The color palette can be changed with \code{density_palette}, replaced with a fixed color using \code{density_fixed_color},
#' or overridden entirely with a custom ggplot2 scale passed via \code{density_scale_custom}.
#'
#' Agreement metrics are calculated using the \code{agreement_metrics()} function and displayed according to \code{metrics_position}.
#' For grouped data with \code{metrics_position = "outside"}, metrics are added to the facet labels; with \code{"inside"}, they are displayed
#' as text annotations within each plot.
#' The choice of placing observed (\code{truth}) values on the x-axis and predicted
#' (\code{estimate}) values on the y-axis follows recommendations from the statistical
#' and ecological modelling literature. Piñeiro et al. (2008) argued that
#' regression and agreement diagnostics are most interpretable when the observed
#' variable is treated as the y axis. More recently, Pauwels et al.
#' (2019) revisited this issue and presented counterarguments supporting the
#' opposite convention. The \code{swap_axes} argument is provided to accommodate both
#' perspectives, with the default setting placing the observed values on the x-axis.
#'
#' Piñeiro, G., Perelman, S., Guerschman, J. P., & Paruelo, J. M. (2008).
#'   How to evaluate models: observed vs. predicted or predicted vs. observed?
#'   Ecological Modelling, 216(3–4), 316–322.
#'
#' Pauwels, V. R. N., Chen, Y., & Sadegh, M. (2019).
#'   Revisiting the observed–predicted scatterplot debate: is the 1:1 line really the best reference?
#'   Ecological Modelling, 407, 108802.
#'
#' @return A ggplot object.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # Example data
#' set.seed(123)
#' df <-
#'   tibble(
#'     truth = c(rnorm(150, 10, 2)),
#'     estimate = truth + rnorm(150, 0, 1),
#'     group = rep(c("A", "B", "C"), each = 50),
#'     group2 = rep(c("D1", "D2"), each = 75)
#'   )
#'
#' # Simple scatterplot
#' scatter(df, truth, estimate)
#'
#' # Scatterplot with agreement metrics (inside plot)
#' scatter(df, truth, estimate, metrics = list("R²" = rsq, mape = mape))
#'
#' # Scatterplot with agreement metrics (outside plot as subtitle)
#' scatter(df, truth, estimate, metrics = list("R²" = rsq, RMSE = rmse), metrics_position = "outside")
#'
#' # Grouped scatterplot with agreement metrics inside
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate, metrics = list("R²" = rsq, RMSE = rmse, "RMSE%" = rrmse), metrics_position = "inside")
#'
#' # Grouped scatterplot with agreement metrics outside as facet labels
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate, metrics = list("R²" = rsq, RMSE = rmse), metrics_position = "outside")
#'
#' # ---------------------------------------------------------------------
#' # Point density coloring & controls
#' # ---------------------------------------------------------------------
#'
#' # Zoom the visible plotting range without changing the agreement metrics
#' scatter(df, truth, estimate, plot_range = c(6, 12))
#'
#'
#' # Force point-density with ABSOLUTE scale (comparable across facets)
#' scatter(df, truth, estimate,
#'         point_style = "pointdensity",
#'         density_scale = "absolute",
#'         density_show_legend = TRUE)
#'
#' # Force point-density with RELATIVE scale (0–1 per facet); legend off
#' df %>%
#'   group_by(group) %>%
#'   scatter(truth, estimate,
#'           point_style = "pointdensity",
#'           density_scale = "relative",
#'           density_show_legend = FALSE)
#'
#' # Change the palette used for density mapping (viridis option)
#' scatter(df, truth, estimate,
#'         point_style = "pointdensity",
#'         density_scale = "absolute",
#'         density_show_legend = TRUE,
#'         density_palette = "plasma")
#'
#'
#' # Provide a CUSTOM ggplot2 color scale (overrides viridis)
#' scatter(df, truth, estimate,
#'         point_style = "pointdensity",
#'         density_scale = "absolute",
#'         density_scale_custom = ggplot2::scale_color_distiller(palette = "Reds"),
#'         density_show_legend = TRUE)
#'
#' # Auto-switch to point-density for larger datasets
#' # (uses 'density_switch_n' threshold; here we keep it small for example)
#' scatter(df, truth, estimate,
#'         point_style = "auto",
#'         density_switch_n = 100)  # switches to pointdensity at n >= 100
#'
#' # Alternative density method & smoothing (neighbors + adjust)
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
  metrics = list(
    "R²" = rsq,
    "bias" = md,
    "bias%" = rmd,
    "RMSE" = rmse,
    "RMSE%" = rrmse
  ),
  metrics_position = "inside",
  metrics_inside_placement = "upperleft",
  point_style = c("point", "pointdensity", "auto"),
  density_scale = c("absolute", "relative"),
  swap_axes = FALSE,
  ...
) {
  point_style <- match.arg(point_style)
  density_scale <- match.arg(density_scale)

  # ---- helpers ----
  `%||%` <- function(x, y) if (is.null(x)) y else x

  facet_formula_from_cols <- function(cols) {
    stats::as.formula(paste("~", paste(cols, collapse = " + ")))
  }

  hvjust_from_placement <- function(where) {
    list(
      hjust = ifelse(where %in% c("upperright", "lowerright"), 1, 0),
      vjust = ifelse(where %in% c("upperleft", "upperright"), 1, 0)
    )
  }

  make_square_anchors <- function(df, facet_cols, x_nm, y_nm) {
    df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(facet_cols))) %>%
      dplyr::summarise(
        rmin = min(c(.data[[x_nm]], .data[[y_nm]]), na.rm = TRUE),
        rmax = max(c(.data[[x_nm]], .data[[y_nm]]), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::uncount(weights = 2, .id = "which") %>%
      dplyr::mutate(
        x_blank = dplyr::if_else(which == 1L, rmin, rmax),
        y_blank = dplyr::if_else(which == 1L, rmin, rmax)
      )
  }

  # ---- ensure columns exist ----
  truth_nm <- rlang::as_name(rlang::ensym(truth))
  est_nm <- rlang::as_name(rlang::ensym(estimate))

  if (!all(c(truth_nm, est_nm) %in% colnames(data))) {
    stop("The specified truth and estimate variables do not exist in the data.")
  }

  # ---- params from ... ----
  extra_params <- list(...)
  points_color <- extra_params$points_color %||% "black"
  points_size <- extra_params$points_size %||% 2
  points_alpha <- extra_params$points_alpha %||% 1
  density_adjust <- extra_params$density_adjust %||% 1
  density_method <- match.arg(
    extra_params$density_method %||% "auto",
    c("auto", "kde2d", "neighbors")
  )
  density_show_legend <- extra_params$density_show_legend %||% FALSE
  density_switch_n <- extra_params$density_switch_n %||% 5000
  facet_scale <- match.arg(
    extra_params$facet_scale %||% "fixed",
    c("fixed", "free")
  )
  plot_range <- extra_params$plot_range %||% NULL

  if (!is.null(plot_range)) {
    if (
      !is.numeric(plot_range) ||
        length(plot_range) != 2 ||
        any(!is.finite(plot_range))
    ) {
      stop(
        "`plot_range` must be a numeric vector of length 2 with finite values."
      )
    }
    plot_range <- sort(plot_range)
  }

  text_background_alpha <- extra_params$text_background_alpha %||% 0.5
  text_size <- extra_params$text_size %||% 10
  metrics_nlines <- extra_params$metrics_nlines %||% 1

  density_palette <- extra_params$density_palette %||% "viridis"
  density_fixed_color <- extra_params$density_fixed_color %||% NULL
  density_scale_custom <- extra_params$density_scale_custom %||% NULL

  add_metrics <- !is.null(metrics)
  is_grouped <- dplyr::is_grouped_df(data)

  # ---- axis mapping ----
  if (isTRUE(swap_axes)) {
    aes_mapping <- ggplot2::aes(x = {{ estimate }}, y = {{ truth }})
    x_nm <- est_nm
    y_nm <- truth_nm
    x_label <- est_nm
    y_label <- truth_nm
  } else {
    aes_mapping <- ggplot2::aes(x = {{ truth }}, y = {{ estimate }})
    x_nm <- truth_nm
    y_nm <- est_nm
    x_label <- truth_nm
    y_label <- est_nm
  }

  # ---- choose style (auto) ----
  if (point_style == "auto") {
    point_style <- if (nrow(data) >= density_switch_n) {
      "pointdensity"
    } else {
      "point"
    }
  }

  if (!is.null(extra_params$points_shape)) {
    points_shape <- extra_params$points_shape
  } else {
    points_shape <- if (point_style == "pointdensity") 16 else 1
  }

  # ---- global range (original behavior) ----
  range_values <- range(data[[truth_nm]], data[[est_nm]], na.rm = TRUE)
  visible_range <- plot_range %||% range_values

  # ---- base plot ----
  p <-
    ggplot2::ggplot(data, aes_mapping) +
    {
      if (point_style == "point") {
        ggplot2::geom_point(
          shape = points_shape,
          size = points_size,
          color = points_color,
          alpha = points_alpha
        )
      } else {
        if (!is.null(density_fixed_color)) {
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
    ggplot2::labs(x = x_label, y = y_label) +
    theme_baseR()

  # ---- coord behavior ----
  # ggplot2 limitation: free facet scales are incompatible with fixed-ratio coords.
  if (!is_grouped || facet_scale == "fixed") {
    p <- p + ggplot2::coord_fixed(xlim = visible_range, ylim = visible_range)
  } else {
    # "square panels" approach: anchors + aspect ratio
    p <- p +
      ggplot2::coord_cartesian(xlim = plot_range, ylim = plot_range) +
      ggplot2::theme(aspect.ratio = 1)
  }

  # ---- density scale (only when mapping color) ----
  if (point_style == "pointdensity" && is.null(density_fixed_color)) {
    if (!is.null(density_scale_custom)) {
      p <- p + density_scale_custom
    } else {
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

  # ---- facets ----
  facet_cols <- character(0)
  groups_count <- 0L

  if (is_grouped) {
    facet_cols <- dplyr::group_vars(data)
    groups_count <- length(facet_cols)

    p <- p +
      ggplot2::facet_wrap(
        facet_formula_from_cols(facet_cols),
        scales = facet_scale
      )

    if (facet_scale == "free" && is.null(plot_range)) {
      anchors <- make_square_anchors(data, facet_cols, x_nm, y_nm)
      p <- p +
        ggplot2::geom_blank(
          data = anchors,
          mapping = ggplot2::aes(x = .data$x_blank, y = .data$y_blank),
          inherit.aes = FALSE
        )
    }
  }

  # ---- metric placement anchors ----
  position_coords <- list(
    upperright = c(max(visible_range), max(visible_range)),
    upperleft = c(min(visible_range), max(visible_range)),
    lowerright = c(max(visible_range), min(visible_range)),
    lowerleft = c(min(visible_range), min(visible_range))
  )

  if (!metrics_inside_placement %in% names(position_coords)) {
    stop(
      "Invalid metrics_inside_placement. Choose from 'upperright', 'upperleft', 'lowerright', or 'lowerleft'."
    )
  }

  hv <- hvjust_from_placement(metrics_inside_placement)
  ann_x_global <- position_coords[[metrics_inside_placement]][1]
  ann_y_global <- position_coords[[metrics_inside_placement]][2]

  # ---- metrics: ungrouped ----
  if (add_metrics && !is_grouped) {
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
      p <- p +
        ggplot2::annotate(
          geom = "richtext",
          x = ann_x_global,
          y = ann_y_global,
          label = metrics_text,
          size = text_size / 2.845276,
          hjust = hv$hjust,
          vjust = hv$vjust,
          fill = scales::alpha(colour = "white", alpha = text_background_alpha),
          label.color = NA
        )
    }

    if (metrics_position == "outside") {
      if (metrics_nlines > 1) {
        parts <- stringr::str_split(metrics_text, ";\\s*")[[1]]
        n_per_line <- ceiling(length(parts) / metrics_nlines)
        grouped_parts <- split(parts, ceiling(seq_along(parts) / n_per_line))
        metrics_text <- paste(
          vapply(
            grouped_parts,
            paste,
            collapse = "; ",
            FUN.VALUE = character(1)
          ),
          collapse = "\n"
        )
      }
      p <- p + ggplot2::labs(subtitle = metrics_text)
    }
  }

  # ---- metrics: grouped ----
  if (add_metrics && is_grouped) {
    metrics_tbl <- agreement_metrics(
      data = data,
      truth = {{ truth }},
      estimate = {{ estimate }},
      metrics = {{ metrics }},
      label = TRUE
    )

    if (metrics_position == "inside") {
      metrics_tbl <- metrics_tbl %>%
        dplyr::mutate(label = stringr::str_replace_all(label, "; ", "<br>"))

      if (facet_scale == "free") {
        if (is.null(plot_range)) {
          per_facet_rng <- data %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(facet_cols))) %>%
            dplyr::summarise(
              rmin = min(c(.data[[x_nm]], .data[[y_nm]]), na.rm = TRUE),
              rmax = max(c(.data[[x_nm]], .data[[y_nm]]), na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              ann_x = if (
                metrics_inside_placement %in% c("upperleft", "lowerleft")
              ) {
                rmin
              } else {
                rmax
              },
              ann_y = if (
                metrics_inside_placement %in% c("upperleft", "upperright")
              ) {
                rmax
              } else {
                rmin
              }
            )
        } else {
          per_facet_rng <- metrics_tbl %>%
            dplyr::select(dplyr::all_of(facet_cols)) %>%
            dplyr::distinct() %>%
            dplyr::mutate(
              ann_x = if (
                metrics_inside_placement %in% c("upperleft", "lowerleft")
              ) {
                min(visible_range)
              } else {
                max(visible_range)
              },
              ann_y = if (
                metrics_inside_placement %in% c("upperleft", "upperright")
              ) {
                max(visible_range)
              } else {
                min(visible_range)
              }
            )
        }

        metrics_tbl <- metrics_tbl %>%
          dplyr::left_join(per_facet_rng, by = facet_cols)
      } else {
        metrics_tbl <- metrics_tbl %>%
          dplyr::mutate(ann_x = ann_x_global, ann_y = ann_y_global)
      }

      p <- p +
        ggtext::geom_richtext(
          data = metrics_tbl,
          ggplot2::aes(x = .data$ann_x, y = .data$ann_y, label = .data$label),
          inherit.aes = FALSE,
          hjust = hv$hjust,
          vjust = hv$vjust,
          size = text_size / 2.845276,
          fill = scales::alpha(colour = "white", alpha = text_background_alpha),
          label.color = NA
        )
    }

    if (metrics_position == "outside") {
      if (groups_count == 1) {
        gv <- facet_cols[[1]]

        metrics_tbl <- metrics_tbl %>%
          dplyr::mutate(
            label = {
              label_parts <- stringr::str_split(label, ";\\s*")
              label_wrapped <- purrr::map_chr(label_parts, function(parts) {
                if (metrics_nlines > 1) {
                  n_per_line <- ceiling(length(parts) / metrics_nlines)
                  grouped_parts <- split(
                    parts,
                    ceiling(seq_along(parts) / n_per_line)
                  )
                  paste(
                    vapply(
                      grouped_parts,
                      paste,
                      collapse = "; ",
                      FUN.VALUE = character(1)
                    ),
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
          metrics_tbl$label,
          metrics_tbl[[gv]]
        ))

        p <- p +
          ggplot2::facet_wrap(
            facet_formula_from_cols(gv),
            scales = facet_scale,
            labeller = custom_labeller
          ) +
          ggplot2::theme(
            strip.text = ggtext::element_textbox(halign = 0.5, size = text_size)
          )

        if (facet_scale == "free" && is.null(plot_range)) {
          anchors <- make_square_anchors(data, facet_cols, x_nm, y_nm)
          p <- p +
            ggplot2::geom_blank(
              data = anchors,
              mapping = ggplot2::aes(x = .data$x_blank, y = .data$y_blank),
              inherit.aes = FALSE
            )
        }
      } else {
        metrics_tbl <- metrics_tbl %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            group_label = paste(
              dplyr::across(dplyr::all_of(facet_cols)),
              collapse = " | "
            ),
            label = {
              parts <- stringr::str_split(label, ";\\s*")[[1]]
              wrapped <- if (metrics_nlines > 1) {
                n_per_line <- ceiling(length(parts) / metrics_nlines)
                grouped_parts <- split(
                  parts,
                  ceiling(seq_along(parts) / n_per_line)
                )
                paste(
                  vapply(
                    grouped_parts,
                    paste,
                    collapse = "; ",
                    FUN.VALUE = character(1)
                  ),
                  collapse = "<br>"
                )
              } else {
                paste(parts, collapse = "; ")
              }
              paste0(group_label, "<br>", wrapped)
            }
          ) %>%
          dplyr::ungroup()

        # safer group_label creation (no !!! inside paste())
        data2 <- data %>%
          dplyr::mutate(
            group_label = purrr::pmap_chr(
              dplyr::across(dplyr::all_of(facet_cols)),
              ~ paste(c(...), collapse = " | ")
            )
          )

        custom_labeller <- ggplot2::as_labeller(stats::setNames(
          metrics_tbl$label,
          metrics_tbl$group_label
        ))

        p <-
          ggplot2::ggplot(data2, aes_mapping) +
          {
            if (point_style == "point") {
              ggplot2::geom_point(
                shape = points_shape,
                size = points_size,
                color = points_color,
                alpha = points_alpha
              )
            } else {
              if (!is.null(density_fixed_color)) {
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
          ggplot2::labs(x = x_label, y = y_label) +
          theme_baseR() +
          {
            if (facet_scale == "fixed") {
              ggplot2::coord_fixed(xlim = visible_range, ylim = visible_range)
            } else {
              ggplot2::coord_cartesian(xlim = plot_range, ylim = plot_range) +
                ggplot2::theme(aspect.ratio = 1)
            }
          } +
          {
            if (point_style == "pointdensity" && is.null(density_fixed_color)) {
              if (!is.null(density_scale_custom)) {
                density_scale_custom
              } else {
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
                  trans = if (density_scale == "absolute") {
                    "sqrt"
                  } else {
                    "identity"
                  }
                )
              }
            }
          } +
          ggplot2::facet_wrap(
            ~group_label,
            scales = facet_scale,
            labeller = custom_labeller
          ) +
          ggplot2::theme(
            strip.text = ggtext::element_textbox(halign = 0.5, size = text_size)
          )

        if (facet_scale == "free" && is.null(plot_range)) {
          anchors <- make_square_anchors(data2, "group_label", x_nm, y_nm)
          p <- p +
            ggplot2::geom_blank(
              data = anchors,
              mapping = ggplot2::aes(x = .data$x_blank, y = .data$y_blank),
              inherit.aes = FALSE
            )
        }
      }
    }
  }

  p
}
