#' Size biased log-normal probability density
#'
#' Calculates size biased log-normal probability density - the underlying
#' log-normal probability density when the likelihood of recording an
#' observation is proportional to its value.
#'
#' @param x A vector of positive numbers.
#' @param lmean The mean of the distribution on the log scale.
#' @param lsig The standard deviation of the distribution on the log scale.
#' @param log A logical indicating whether to return log densities.
#' @param xlog A logical indicating whether to return densities for log
#'  transformed data.
#' @return A vector of probability densities.
#' @examples
#'   data(data)
#'   dsblnorm(data$speed, 0, 0.1)
#' @export
#'
dsblnorm <- function(x, lmean, lsig, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dlnorm(x, lmean-exp(lsig)^2/2, exp(lsig)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}
