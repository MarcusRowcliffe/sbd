#' Summarise a linear model
#'
#' For parametric models only, summarises the linear model coefficients from a
#' an \code{sbm} object.
#'
#' @param object A size biased model fit of class \code{\link{sbm}}.
#' @param ... Additional arguments (unused).
#' @return A dataframe with fields \code{estimate}, \code{stdError},
#'  \code{tValue}, and \code{pValue}.
#' @examples
#'   data(BCI_speed_data)
#'   mod <- sbm(speed~mass, BCI_speed_data, pdf="lnorm")
#'   summary(mod)
#' @export
#'
summary.sbm <- function(object, ...){
  if(object$pdf == "none") NULL else{
    cf <- object$model@coef
    se <- sqrt(diag(object$model@vcov))
    tval <- abs(cf / se)
    df <- nrow(object$data) - length(cf)
    pval <- 2 * pt(tval, df, lower.tail=FALSE)
    data.frame(estimate = cf, stdError = se, tValue = tval, pValue = pval)
  }
}
