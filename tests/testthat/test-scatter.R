test_that("scatter validates required inputs", {
  df <- make_scatter_data()

  expect_error(
    scatter(df, missing_truth, estimate),
    "do not exist"
  )

  expect_error(
    scatter(df, truth, estimate, plot_range = c(1, NA)),
    "plot_range"
  )

  expect_error(
    scatter(df, truth, estimate, metrics_inside_placement = "middle"),
    "Invalid metrics_inside_placement"
  )

  expect_error(
    scatter(df, truth, estimate, metrics_nlines = 0),
    "metrics_nlines"
  )
})

test_that("scatter returns ggplot objects across core display modes", {
  df <- make_scatter_data()

  p_basic <- scatter(df, truth, estimate)
  expect_s3_class(p_basic, "ggplot")
  expect_equal(p_basic$labels$x, "truth")
  expect_equal(p_basic$labels$y, "estimate")

  p_swapped <- scatter(df, truth, estimate, swap_axes = TRUE)
  expect_equal(p_swapped$labels$x, "estimate")
  expect_equal(p_swapped$labels$y, "truth")

  p_no_metrics <- scatter(df, truth, estimate, metrics = NULL)
  expect_s3_class(p_no_metrics, "ggplot")
  expect_null(p_no_metrics$labels$subtitle)

  p_outside <- scatter(
    df,
    truth,
    estimate,
    metrics = list("R2" = yardstick::rsq, RMSE = yardstick::rmse),
    metrics_position = "outside",
    metrics_nlines = 2
  )
  expect_match(p_outside$labels$subtitle, "R2:")
  expect_match(p_outside$labels$subtitle, "\n", fixed = TRUE)

  p_inside <- scatter(
    df,
    truth,
    estimate,
    metrics_position = "inside",
    plot_range = c(0, 25),
    points_shape = 3
  )
  expect_true(any(vapply(p_inside[["layers"]], function(x) class(x[["geom"]])[1] == "GeomRichText", logical(1))))
})

test_that("scatter covers density variants and free-facet branches", {
  df <- make_scatter_data()

  p_density_fixed <- scatter(
    df,
    truth,
    estimate,
    point_style = "pointdensity",
    density_fixed_color = "red",
    density_adjust = 1.2,
    density_method = "neighbors",
    density_show_legend = TRUE
  )
  expect_s3_class(p_density_fixed, "ggplot")

  p_density_scale <- scatter(
    df,
    truth,
    estimate,
    point_style = "pointdensity",
    density_scale = "relative",
    density_scale_custom = ggplot2::scale_color_distiller(palette = "Reds")
  )
  expect_s3_class(p_density_scale, "ggplot")

  p_auto <- scatter(
    df,
    truth,
    estimate,
    point_style = "auto",
    density_switch_n = 1
  )
  expect_s3_class(p_auto, "ggplot")

  p_auto_point <- scatter(
    df,
    truth,
    estimate,
    point_style = "auto",
    density_switch_n = 1000
  )
  expect_s3_class(p_auto_point, "ggplot")

  p_grouped_inside <- df |>
    dplyr::group_by(group) |>
    scatter(
      truth,
      estimate,
      metrics_position = "inside",
      facet_scale = "free",
      metrics_inside_placement = "lowerright"
    )
  expect_s3_class(p_grouped_inside, "ggplot")

  p_grouped_inside_upperleft <- df |>
    dplyr::group_by(group) |>
    scatter(
      truth,
      estimate,
      metrics_position = "inside",
      facet_scale = "free",
      metrics_inside_placement = "upperleft"
    )
  expect_s3_class(p_grouped_inside_upperleft, "ggplot")

  p_grouped_inside_limited <- df |>
    dplyr::group_by(group) |>
    scatter(
      truth,
      estimate,
      metrics_position = "inside",
      facet_scale = "free",
      plot_range = c(0, 25),
      metrics_inside_placement = "upperleft"
    )
  expect_s3_class(p_grouped_inside_limited, "ggplot")

  p_grouped_inside_limited_lowerright <- df |>
    dplyr::group_by(group) |>
    scatter(
      truth,
      estimate,
      metrics_position = "inside",
      facet_scale = "free",
      plot_range = c(0, 25),
      metrics_inside_placement = "lowerright"
    )
  expect_s3_class(p_grouped_inside_limited_lowerright, "ggplot")

  p_grouped_inside_fixed <- df |>
    dplyr::group_by(group) |>
    scatter(
      truth,
      estimate,
      metrics_position = "inside",
      facet_scale = "fixed"
    )
  expect_s3_class(p_grouped_inside_fixed, "ggplot")

  p_grouped_outside_single <- df |>
    dplyr::group_by(group) |>
    scatter(
      truth,
      estimate,
      metrics_position = "outside",
      facet_scale = "free",
      metrics_nlines = 2
  )
  expect_s3_class(p_grouped_outside_single, "ggplot")

  strip_labeller <- p_grouped_outside_single$facet$params$labeller
  strip_label <- strip_labeller(
    data.frame(group = unique(df$group)[1], stringsAsFactors = FALSE)
  )[[1]][[1]]
  expect_match(strip_label, "<br>", fixed = TRUE)
  expect_match(strip_label, "font-size:9pt", fixed = TRUE)

  p_grouped_outside_single_default_lines <- df |>
    dplyr::group_by(group) |>
    scatter(
      truth,
      estimate,
      metrics_position = "outside",
      facet_scale = "free",
      metrics_nlines = 1
    )
  expect_s3_class(p_grouped_outside_single_default_lines, "ggplot")

  p_grouped_outside_multi_point <- df |>
    dplyr::group_by(group, group2) |>
    scatter(
      truth,
      estimate,
      metrics_position = "outside",
      facet_scale = "fixed",
      point_style = "point"
    )
  expect_s3_class(p_grouped_outside_multi_point, "ggplot")

  p_grouped_outside_multi_fixed_color <- df |>
    dplyr::group_by(group, group2) |>
    scatter(
      truth,
      estimate,
      metrics_position = "outside",
      facet_scale = "fixed",
      metrics_nlines = 2,
      point_style = "pointdensity",
      density_fixed_color = "red"
    )
  expect_s3_class(p_grouped_outside_multi_fixed_color, "ggplot")

  p_grouped_outside_multi_custom <- df |>
    dplyr::group_by(group, group2) |>
    scatter(
      truth,
      estimate,
      metrics_position = "outside",
      facet_scale = "fixed",
      point_style = "pointdensity",
      density_scale_custom = ggplot2::scale_color_distiller(palette = "Reds")
    )
  expect_s3_class(p_grouped_outside_multi_custom, "ggplot")

  p_grouped_outside_multi_absolute <- df |>
    dplyr::group_by(group, group2) |>
    scatter(
      truth,
      estimate,
      metrics_position = "outside",
      facet_scale = "fixed",
      point_style = "pointdensity",
      density_scale = "absolute"
    )
  expect_s3_class(p_grouped_outside_multi_absolute, "ggplot")

  p_grouped_outside_multi_relative <- df |>
    dplyr::group_by(group, group2) |>
    scatter(
      truth,
      estimate,
      metrics_position = "outside",
      facet_scale = "free",
      metrics_nlines = 2,
      point_style = "pointdensity",
      density_scale = "relative"
    )
  expect_s3_class(p_grouped_outside_multi_relative, "ggplot")

  p_relative_scale <- scatter(
    df,
    truth,
    estimate,
    point_style = "pointdensity",
    density_scale = "relative"
  )
  expect_s3_class(p_relative_scale, "ggplot")
})
