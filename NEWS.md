# scatter 0.1.14 (2026-06-15)
- Added `n_obs()` / `n_obs_vec()` as yardstick-compatible metrics for counting paired observations.
- Added `metric_pair()` and `metric_format()` to control how agreement metrics are displayed in labels while preserving numeric metric columns.
- Updated default agreement metrics to show paired bias and RMSE values with their relative percentages in labels.
- Added `metrics_nlines` support for splitting outside metric labels across multiple lines.
- Updated examples, README, documentation, and tests for the new metric display features.

# scatter 0.1.13 (2026-04-08)
- Added `plot_range` to zoom the visible square plotting window without changing full-data agreement metrics, and to keep inside-plot metric annotations anchored within the visible panel.
- Moved secondary tuning and layout controls such as `plot_range`, density controls, and `facet_scale` into `...`, leaving the main function signature focused on core behavior.
- Added a `testthat` test suite with full package coverage.

# scatter 0.1.12
- Axis orientation — truth (observed) is now placed on the x-axis by default, with an option to restore the previous behavior using swap_axes = TRUE. Axis orientation affects only the visual layout; agreement metrics are computed consistently as metric(truth, estimate).
- Default point shapes enhanced — when point_style = "pointdensity", the default shape is now a filled circle (shape = 16) for improved visibility. For regular point mode, the default remains an open circle (shape = 1). User-supplied shapes continue to override both defaults.

# scatter 0.1.11
- Added support for density-based scatter plots using `geom_pointdensity()`.
- New arguments: `point_style`, `density_scale`, and extended color control via `...`.
- Improved legend and color scaling behavior across facets.
