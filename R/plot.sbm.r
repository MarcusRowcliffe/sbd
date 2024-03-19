#' Plot size biased distribution
#'
#' Plots the frequency distribution of data from a size biased model, with
#' fitted parametric distribution if the model is parametric and used no
#' covariates.
#'
#' @param mod A size biased model fit of class \code{\link{sbm}}.
#' @param log A logical specifying whether to plot the log transformed or
#'  untransformed data distribution.
#' @param add A logical specifying whether to create a new plot, or add a
#'  parametric distribution curve to an existing plot.
#' @param ppar A list of plotting parameters passed to plot; accepts
#'  arguments controlling both overall plot appearance and graphical elements
#'  of the histogram.
#' @param hpar A list of parameters used when creating the frequency data;
#'  primarily used to determine breaks.
#' @param lpar A list of plotting parameters controlling appearance of the
#'  density curve line (if present).
#' @return None.
#' @examples
#'   data(sbdData)
#'   lmod <- sbm(speed~1, sbdData, pdf="lnorm")
#'   wmod <- sbm(speed~1, sbdData, pdf="weibull")
#'   plot(lmod, hpar=list(breaks = 40))
#'   plot(wmod, add=TRUE, lpar=list(col="blue"))
#' @export
#'
plot.sbm <- function(mod, log=TRUE, add=FALSE,
                     ppar=list(col=NULL), hpar=list(), lpar=list(col="red")){
  allvars <- all.vars(mod$formula)
  x <- get(allvars[1], mod$data)
  if(log) x <- log(x)

  if(mod$pdf != "none" & length(allvars) == 1){
    cfs <- coef(mod$model)
    sq <- seq(min(x), max(x), len=256)
    if(log) sq <- exp(sq)
    den <- switch(mod$pdf,
                  gamma = dsbgamma(sq, cfs[1], cfs[2], xlog=log),
                  lnorm = dsblnorm(sq, cfs[1], cfs[2], xlog=log),
                  weibull = dsbweibull(sq, cfs[1], cfs[2], xlog=log))
    if(log) sq <- log(sq)
    mx <- max(den)
  } else mx <- 0

  if(!add){
    h <- do.call(hist, c(list(x=x, plot=FALSE), hpar))
    ppar <- c(list(x=h, freq=FALSE), ppar)
    pargs <- names(ppar)
    if(!"main" %in% pargs) ppar <- c(ppar, main="")
    if(!"xlab" %in% pargs) ppar <- c(ppar, xlab=allvars[1])
    if(!"ylim" %in% pargs){
      lim <- range(c(0, mx, h$density))
      ppar <- c(ppar, list(ylim=lim))
    }
    do.call(plot, ppar)
  }

  if(mod$pdf != "none" & length(allvars) == 1){
    do.call(lines, c(list(x=sq, y=den), lpar))
  }
}
