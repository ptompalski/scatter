#' print n random rows of a data frame
#'
#' @param df a data frame.
#' @param n number of rows to print. Default = 5
#' @return \code{n} random rows of \code{df}.
#' @examples
#' mid(iris)
#' mid(iris, 10)
#' @export

mid <- function(df,n=5) {
  return(df[sample(nrow(df),n),])
}


#' round a number to nearest given integer
#'
#' @param x numeric value, can be a vector.
#' @param base integer to round to. Default = 10
#' @return \code{x} rounded to nearest \code{base}.
#' @examples
#' mround(11.32)
#' mround(seq(1,10,by=0.5),2)
#' @export

mround <- function(x,base=10){
  base*round(x/base)
}


#' Calculate root mean square error
#'
#' @param x a single value or vector of observed (reference) values
#' @param y a single value of vector of the same length as \code{x} of predicted values
#' @return root meas square error
#' @examples
#' rmse(x,y)
#' @export

rmse <- function(x,y) {
  sqrt(mean((x - y)^2, na.rm = TRUE))
}


#' Calculate most frequent value of a vector
#'
#' @param x a vector
#' @return mode value of \code{x}
#' @examples
#' mode(rnorm(20))
#' @export


mode <- function(x) {
  if(is.numeric(x)) {
    x <- round(x,2)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Calculate R2
#'
#' @param obs a vector of reference value
#' @param pred a vector of estimated (predicted) values
#' @return R2 value
#' @export

r_square <- function(obs,pred) {
  resid <- obs - pred
  1 - (var(resid, na.rm = TRUE) / var(obs, na.rm = TRUE))
}



