#' Get AIC of size biased model
#'
#' Extracts the Akaike Information Criterion from a fitted parametric size
#' biased distribution model.
#'
#' @param object A size biased distribution model of class \code{sbm}.
#' @param ... Optional additional \code{sbm} objects.
#' @param k Numeric, the penalty per parameter to be used; the default k = 2
#'  is the classical AIC.
#' @return A dataframe of AIC values, NA if the model is non-parametric
#'  (i.e. fitted with \code{pdf = "none"}).
#' @examples
#'   data(BCI_speed_data)
#'   lmod_null <- sbm(speed~1, BCI_speed_data, pdf="lnorm")
#'   lmod_mass <- sbm(speed~mass, BCI_speed_data, pdf="lnorm")
#'   AIC(lmod_null, lmod_mass)
#' @export
#'
AIC.sbm <- function(object, ..., k=2){
  mods <- c(list(object), list(...))
  classes <- unlist(lapply(mods, class))

  if(!all(classes == "sbm")) stop("All arguments must be class sbm")

  modnms <- c(deparse(substitute(object)),
              unlist(lapply(substitute(list(...))[-1],
                            deparse)))
  aics <- unlist(lapply(mods, function(m)
    if(m$pdf=="none") NA else bbmle::AIC(m$model, k=k)))
  pdfs <- unlist(lapply(mods, function(m) m$pdf))
  res <- data.frame(model = modnms,
                    PDF = pdfs,
                    AIC = aics,
                    dAIC = aics-min(aics, na.rm=TRUE))
  res[order(res$dAIC), ]
}
