#' scatterplot with information on the errors between x and y.
#'
#' @param x a vector of observed data.
#' @param y a vector of predicted data.
#' @param R2 Optional. Can be enabled or disabled by setting TRUE/FALSE. Can also be a value in cases where the R2 is calculated by other funtion. Can also be "cor" to calculate by squaring the correlation coefficient.
#' @param axisorder Optional. Set to \code{PO} (predicted-observed) to plot predicted (\code{y}) on the y-axis (this is the default). Set to \code{OP} (observed-predicted) to plot observed (\code{x}) on the y-axis.
#' @param xlab Optional. Title of the x-axis
#' @param ylab Optional. Title of the y-axis
#' @param info A logical value indictating whether information on count, bias and RMSE should be added to the plot.
#' @param position Determines the position of the info box
#' @param positionauto A logical value indicating whether the position of the info box should be optimized automaticaly.
#' @param lowerlimit A value determining the lower limit of the x and y axis
#' @param upperlimit A value determining the upper limit of the x and y axis
#' @param alpha Define the transparency of the points. 0 - fully transparent, 1 - opaque.
#' @param add.reg.line Logical. Should the regression line be added to the plot? Regression coeficients are calculated automatically.
#' @param rug Logical. Add marginal rug to the plot.
#' @param label_text A character vector of length=5 defining the names for the values in the info box.
#' @return a scatterplot of \code{x} and \code{y}.
#' @description This scatterplot is a wrapper function for a ggplot-based plot. It containes additional text panel that shows values calculated with \code{\link{calc.error}}
#' @examples
#' x <- iris$Sepal.Length
#' y <- predict(lm(data=iris,iris$Sepal.Length~iris$Petal.Width))
#' scatter(x,y)
#' @export




scatter <- function (x, y, R2=T, axisorder = "OP", xlab = "Observed",
                      ylab = "Predicted", title = NULL, info = T, position = 0,
                      positionauto = T, lowerlimit = NA, upperlimit = NA, alpha = 1, normality=T,
                      add.reg.line = F, rug = F, label_text = c("n", "bias", "bias%",
                                                                "RMSE", "RMSE%","p-value"))
{

    data <- data.frame(x = x, y = y)
    pts <- ggplot2::geom_point(shape = 1, size = 2, alpha = alpha)


  if(normality == T) {
    d <- scatter::calc.error(reference = data$x, estimate = data$y)
  } else {
    d <- scatter::calc.error(reference = data$x, estimate = data$y, dist.normal = F)
  }

  label <- paste(#label_text[1], " = ", d$count, "\n",
    label_text[2], " = ", round(d$bias, 3), "\n",
    label_text[3], " = ", round(d$bias_perc, 2), "\n",
    label_text[4], " = ", round(d$RMSE, 3), "\n",
    label_text[5], " = ", round(d$RMSE_perc, 2), "\n",
    label_text[6], " = ", round(d$p_val, 3),
    sep = "")


  if(R2 == T) {
    R2 <- r_square(obs = x, pred = y)
    label <- paste0("R2 = ", round(R2,2),
                    "\n",
                    label
    )
  } else if (R2=="cor") {
    R2 <- cor(x,y)^2
    label <- paste0("R2 = ", round(R2,2),
                    "\n",
                    label
    )
  } else if (is.numeric(R2)) {
    label <- paste0("R2 = ", round(R2,2),
                    "\n",
                    label
    )
  }

  # if (!is.null(R2)) {
  #   label <- paste0("R2 = ", round(R2,2),
  #                  "\n",
  #                  label
  #                  )
  # }
  #


  if (axisorder == "OP") {
    data <- data.frame(x = data$y, y = data$x)
    x_lab_copy <- xlab
    xlab = ylab
    ylab = x_lab_copy
  }
  if (is.na(lowerlimit))
    lowerlimit <- min(data[c("x", "y")], na.rm = T)
  if (is.na(upperlimit))
    upperlimit <- max(data[c("x", "y")], na.rm = T)
  if (position != 0)
    positionauto <- F
  if (positionauto == T) {
    if (is.finite(d$bias_perc) & d$bias_perc < -20)
      position <- 1
  }
  if (position == 0) {
    ann_x <- upperlimit
    ann_y <- -Inf
    ann_hjust <- 1
    ann_vjust <- -0.2
  }
  if (position == 1) {
    ann_x <- lowerlimit
    ann_y <- upperlimit
    ann_hjust <- 0
    ann_vjust <- 0.9
  }
  if (info == T) {
    ann <- ggplot2::annotate("text", x = ann_x, y = ann_y,
                             label = label, hjust = ann_hjust, vjust = ann_vjust)
  }
  else {
    ann <- bw
  }
  if (rug == T) {
    addrug <- ggplot2::geom_rug(alpha = 0.2)
  }
  else {
    addrug <- bw
  }
  if (add.reg.line == T) {
    reg.line <- ggplot2::geom_smooth(se = FALSE, method = "lm",
                                     colour = "red")
  }
  else {
    reg.line <- bw
  }
  plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x,
                                                    y = y)) + pts + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::ggtitle(title) + ggplot2::xlim(lowerlimit, upperlimit) +
    ggplot2::ylim(lowerlimit, upperlimit) + ggplot2::geom_abline(intercept = 0,
                                                                 slope = 1) + ann + ggplot2::theme(legend.position = "bottom") +
    ggplot2::coord_equal(ratio = 1) + addrug + reg.line +
    scatter:::bw
  return(plot)
}
