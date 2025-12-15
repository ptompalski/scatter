# scatter 0.1.12
- Axis orientation — truth (observed) is now placed on the x-axis by default, with an option to restore the previous behavior using swap_axes = TRUE. Axis orientation affects only the visual layout; agreement metrics are computed consistently as metric(truth, estimate).
- Default point shapes enhanced — when point_style = "pointdensity", the default shape is now a filled circle (shape = 16) for improved visibility. For regular point mode, the default remains an open circle (shape = 1). User-supplied shapes continue to override both defaults.

# scatter 0.1.11
- Added support for density-based scatter plots using `geom_pointdensity()`.
- New arguments: `point_style`, `density_scale`, and extended color control via `...`.
- Improved legend and color scaling behavior across facets.