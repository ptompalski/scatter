% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h.R
\name{h}
\alias{h}
\title{A histogram with descriptive statistics}
\usage{
h(x, binwidth = NULL, xlab = NULL, ylab = "count", lowerlimit = NA,
  upperlimit = NA, info = T, density = F, perc = F, fontsize = 4,
  text_horizontal_position = "right", fill_color = "#d3d3d3")
}
\arguments{
\item{x}{a vector}

\item{binwidth}{Optional. width of a histogram bin.}

\item{xlab}{Optional. X-axis label.}

\item{ylab}{Optional. Y-axis label.}

\item{lowerlimit}{Optional. Lower limit of the histogram}

\item{upperlimit}{Optional. Upper limit of the histogram}

\item{info}{Optional. Logical - should the descriptive stats be ploted?}

\item{density}{Optional. Logical - Plot density plot instead of histogram.}

\item{perc}{Optional. Logical - Should the counts for each bin be converted to percent?}

\item{text_horizontal_position}{Optional. Horizontal position of the info box. Default = "right". Can be "left" or a numerical value indicating the position of the left side of the box.}

\item{fill_color}{Color of the histogram bars.}

\item{fontize}{Optional. Fontsize of the plot text elements.}
}
\value{
histogram of \code{x}
}
\description{
This custom histogram is a wrapper function for a custiomized ggplot generated histogram. Additional descriptive statistics are calculated with function \code{\link{des}}.
}
\examples{
h(iris$Sepal.Length)
}
