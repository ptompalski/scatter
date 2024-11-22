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
theme_baseR <- function(font_size = 12, font_family = "sans") {
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = font_size * 0.9, family = font_family),
    axis.title = element_text(color = "black", size = font_size, family = font_family),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = font_size * 1.2, family = font_family),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = font_size * 0.8, family = font_family),
    legend.title = element_text(size = font_size * 0.9, family = font_family),
    strip.background = element_blank(),
    strip.text = element_text(size = font_size, family = font_family)
    
  )
}
