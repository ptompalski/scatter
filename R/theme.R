#' Custom ggplot Theme
#'
#' A minimalist custom ggplot theme with configurable font size and font family.
#'
#' @param font_size Numeric. The base font size for text elements in the plot. Default is 12.
#' @param font_family Character. The font family to use for all text elements. Default is "sans".
#' @return A ggplot2 theme object.
#'
#' @examples
#' library(ggplot2)
#' 
#' # Example plot with the default theme
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_baseR()
#'
#' # Custom font size and font family
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_baseR(font_size = 14, font_family = "serif")
#'
#' @export
theme_baseR <- function(font_size = 10, font_family = "sans") {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = "black", fill = NA),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(color = "black"),
    axis.text = ggplot2::element_text(color = "black", size = font_size * 0.9, family = font_family),
    axis.title = ggplot2::element_text(color = "black", size = font_size, family = font_family),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = font_size * 1.2, family = font_family),
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.key = ggplot2::element_rect(fill = "white", color = NA),
    legend.text = ggplot2::element_text(size = font_size * 0.8, family = font_family),
    legend.title = ggplot2::element_text(size = font_size * 0.9, family = font_family),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = font_size, family = font_family)
    
  )
}
