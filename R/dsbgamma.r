#' Size biased gamma probability density
#'
#' Calculates size biased gamma probability density - the underlying
#' gamma probability density when the likelihood of recording an
#' observation is proportional to its value.
#'
#' @param x A vector of positive numbers.
#' @param lmean The mean of the distribution on the log scale.
#' @param lrate The log rate parameter of the underlying gamma distribution.
#' @param log A logical indicating whether to return log densities.
#' @param xlog A logical indicating whether to return densities for log
#'  transformed data.
#' @return A vector of probability densities.
#' @examples
#'   data(data)
#'   dsbgamma(data$speed, 0, 0.1)
#' @export
dsbgamma <- function(x, lmean, lrate, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dgamma(x, exp(lmean)*exp(lrate), exp(lrate)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}
