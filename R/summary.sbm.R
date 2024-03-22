#' Summarise a linear model
#'
#' For parametric models only, summarises the linear model parameter estimates.
#'
#' @param mod A size biased model fit of class \code{\link{sbm}}.
#' @return A dataframe with fields \code{estimate}, \code{stdError},
#'  \code{tValue}, and \code{pValue}.
#' @examples
#'   data(BCI_speed_data)
#'   mod <- sbm(speed~mass, BCI_speed_data, pdf="lnorm")
#'   summary(mod)
#' @export
#'
summary.sbm <- function(mod){
  if(mod$pdf == "none") NULL else{
    cf <- mod$model@coef
    se <- sqrt(diag(mod$model@vcov))
    tval <- abs(cf / se)
    df <- nrow(mod$data) - length(cf)
    pval <- 2 * pt(tval, df, lower=FALSE)
    data.frame(estimate = cf, stdError = se, tValue = tval, pValue = pval)
  }
}
