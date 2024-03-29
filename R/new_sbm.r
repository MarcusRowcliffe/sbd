#' sbm (size biased model) class constructor
#'
#' @param x a list with required elements as indicated
#' @noRd
#'
new_sbm <- function(x = list()) {
  required <- c("data", "estimate", "formula", "model", "pdf")
  stopifnot(is.list(x))
  stopifnot(identical(required, sort(names(x))))
  structure(x, class = "sbm")
}
