#' Get AIC of size biased model
#'
#' Extracts the Akaike Information Criterion from a fitted parametric size
#' biased distribution model.
#'
#' @param ... One or more size biased models of class \code{sbm}.
#' @return A dataframe of AIC values, NA if the model is non-parametric
#'  (fitted with \code{pdf = "none"}).
#' @examples
#'   data(BCI_speed_data)
#'   lmod_null <- sbm(speed~1, BCI_speed_data, pdf="lnorm")
#'   lmod_mass <- sbm(speed~mass, BCI_speed_data, pdf="lnorm")
#'   AIC.sbm(lmod_null, lmod_mass)
#' @export
#'
AIC.sbm <- function(...){
  mods <- list(...)
  classes <- unlist(lapply(mods, class))

  if(!all(classes == "sbm")) stop("All arguments must be class sbm")

  models <- unlist(lapply(substitute(list(...))[-1], deparse))
  aics <- unlist(lapply(mods, function(m)
    if(m$pdf=="none") NA else bbmle::AIC(m$model)))
  pdfs <- unlist(lapply(mods, function(m) m$pdf))
  res <- data.frame(model = models,
                    PDF = pdfs,
                    AIC = aics,
                    dAIC = aics-min(aics, na.rm=TRUE))
  res[order(res$dAIC), ]
}
