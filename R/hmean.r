#' Harmonic Mean
#'
#' Calculates harmonic mean and its standard error.
#'
#' @param x A vector of positive numbers.
#' @param na.rm A logical (\code{TRUE} or \code{FALSE}) indicating whether
#'  to strip out missing values before computing.
#' @return A list with values \code{mean} and \code{se}.
#' @examples
#'   data(BCI_speed_data)
#'   agoutiData <- subset(BCI_speed_data, species=="agouti")
#'   hmean(agoutiData$speed)
#' @export
#'
hmean <- function(x, na.rm=TRUE){
  if(any(x <= 0))
    return(list(mean=NA, se=NA)) else{
      if(na.rm) x <- na.omit(x)
      mn <- 1/mean(1/x)
      se <- mn^2 * sqrt(var(1/x)/length(x))
      return(list(mean=mn, se=se))
    }
}
