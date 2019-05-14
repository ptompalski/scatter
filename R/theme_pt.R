#' Custom ggplot template
#'
#' @param base_size Optional font size.
#' @param base_family Optional font name.
#' @return A ggplot template.
#' @aliases bw
#' @export


theme_pt <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) + #%+replace%
    ggplot2::theme(
      #panel.border = ggplot2::element_rect(colour = "black", fill = F, size = 1),
      axis.text = ggplot2::element_text(margin = ggplot2::margin(10,10,10,10)),
      plot.margin = grid::unit(c(1.2, 1.2, 1.2, 1.2), "lines"),
      axis.ticks.length= grid::unit(0.15,"cm"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = F, size = 1),
      legend.key = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10,0,0,0)),#element_text(hjust=0.5,vjust=0.5),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,10,0,0),angle=90),#(hjust=0.5,vjust=1.5,angle=90),
      #strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(lineheight=1.5),
      strip.background = ggplot2::element_blank(),#no border for facet titles
      legend.position="bottom", # legend on bottom
      legend.title = ggplot2::element_blank () #no title for legend
    )
}

bw<-scatter:::theme_pt()