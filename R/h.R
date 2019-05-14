

#' A histogram with descriptive statistics
#'
#' @param x a vector
#' @param binwidth Optional. width of a histogram bin.
#' @param ylab Optional. Y-axis label.
#' @param xlab Optional. X-axis label.
#' @param lowerlimit Optional. Lower limit of the histogram
#' @param upperlimit Optional. Upper limit of the histogram
#' @param info Optional. Logical - should the descriptive stats be ploted?
#' @param density Optional. Logical - Plot density plot instead of histogram.
#' @param perc Optional. Logical - Should the counts for each bin be converted to percent?
#' @param fontize Optional. Fontsize of the plot text elements.
#' @param text_horizontal_position Optional. Horizontal position of the info box. Default = "right". Can be "left" or a numerical value indicating the position of the left side of the box.
#' @param fill_color Color of the histogram bars.
#' @description This custom histogram is a wrapper function for a custiomized ggplot generated histogram. Additional descriptive statistics are calculated with function \code{\link{des}}.

#' @return histogram of \code{x}
#' @examples
#' h(iris$Sepal.Length)
#' @export

h <- function(x,
              binwidth=NULL,
              xlab=NULL,
              ylab="count",
              lowerlimit=NA,
              upperlimit=NA,
              info=T,
              density=F,
              perc=F,
              fontsize=4,
              text_horizontal_position = "right",
              fill_color = "#d3d3d3") {
  
  plot_theme <- theme_pt()
  
  if (!is.na(upperlimit)) {
    x <- x[x <= upperlimit]
  }
  if (!is.na(lowerlimit)) {
    x <- x[x >= lowerlimit]
  }
  
  d <- UsefulRFunctions::des(x)
  
  label <- paste("n = ",d$n,
                 "\nMean = ",round(d$Mean,2),
                 "\nMedian = ",round(d$Median,2),
                 "\nStd.Dev = ",round(d$Std.Dev,2),
                 "\nMin = ",round(d$Min,2),
                 "\nMax = ",round(d$Max,2),
                 sep="")
  
  
  
  
  if (tolower(text_horizontal_position) %in% c("right","r")) {
    upperlimit_ann <- max(x,na.rm=T)
    ann_x <- upperlimit_ann; ann_y <-Inf; ann_hjust <- 1; ann_vjust <- 1.2
  } else if (tolower(text_horizontal_position) %in% c("left","l")) {
    upperlimit_ann <- min(x,na.rm=T)
    ann_x <- upperlimit_ann; ann_y <-Inf; ann_hjust <- 0; ann_vjust <- 1.2
  } else if (is.numeric(text_horizontal_position)) {
    upperlimit_ann <- text_horizontal_position
    ann_x <- upperlimit_ann; ann_y <-Inf; ann_hjust <- 0; ann_vjust <- 1.2
  }
  
  if (info == T) {ann <- ggplot2::annotate("text",x=ann_x,y=ann_y,label=label,hjust=ann_hjust,vjust=ann_vjust,size=fontsize)} else {ann<-bw}
  
  if(perc == T) {
    plot.hist <- ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..)),
                                   colour="black",fill=fill_color,
                                   position = position_dodge(width = 1))
  } else {
    if(density == F) {
      plot.hist <- ggplot2::geom_histogram(binwidth=binwidth,colour="black",fill=fill_color)
    } else {
      plot.hist <- ggplot2::geom_density(colour="black",fill=fill_color)
    }
  }
  
  plot <- ggplot2::ggplot(data=NULL,ggplot2::aes(x=x))+
    plot_theme +
    plot.hist+
    ggplot2::xlab(xlab)+
    ggplot2::ylab(ylab)+
    ann#+
  #ggplot2::xlim(lowerlimit,upperlimit)
  
  return(plot)
}


