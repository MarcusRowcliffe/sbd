#'
#' print method for sbm objects
#'
#' @param x An sbm object
#' @noRd
#'
print.sbm <- function(x, ...){
  writeLines(c("Call:", format(x$formula),
             "\nProbability distribution:", x$pdf,
             "\nEstimates:"))
  print(x$estimate)
}
