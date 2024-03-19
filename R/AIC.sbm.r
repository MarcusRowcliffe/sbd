#' Get AIC of size biased model
#'
#' Extracts the Akaike Information Criterion from a fitted parametric size
#' biased distribution model.
#'
#' @param mod A size biased model fit of class \code{\link{sbm}}.
#' @return A numeric, AIC value, or NULL if the model is non-parametric
#'  (fitted with \code{pdf = "none"}).
#' @examples
#'   data(sbdData)
#'   lmod1 <- sbm(speed~1, sbdData, pdf="lnorm")
#'   lmod2 <- sbm(speed~cov1, sbdData, pdf="lnorm")
#'   AIC(lmod1)
#'   AIC(lmod2)
#' @export
#'
AIC.sbm <- function(mod){
  if(mod$pdf == "none")
    NULL else
      AIC(mod$model)
}
