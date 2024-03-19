#' Size biased Weibull probability density
#'
#' Calculates size biased Weibull probability density - the underlying
#' Weibull probability density when the likelihood of recording an
#' observation is proportional to its value.
#'
#' @param x A vector of positive numbers.
#' @param lmean The mean of the distribution on the log scale.
#' @param lshape The log shape parameter of the underlying Weibull distribution.
#' @param log A logical indicating whether to return log densities.
#' @param xlog A logical indicating whether to return densities for log
#'  transformed data.
#' @return A vector of probability densities.
#' @examples
#'   data(sbdData)
#'   dsbweibull(sbdData$speed, 0, 0.1)
#' @export
#'
dsbweibull <- function(x, lmean, lshape, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dweibull(x, exp(lshape), exp(lmean)/gamma(1+1/exp(lshape))) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}
