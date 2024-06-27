#' Plot size biased distribution
#'
#' Plots the frequency histogram of data from a size biased model, with
#' fitted parametric distribution line if the model is parametric without
#' covariates.
#'
#' @param x A size biased model fit of class \code{\link{sbm}}.
#' @param log A logical specifying whether to plot the log transformed or
#'  untransformed data distribution.
#' @param add A logical specifying whether to create a new plot, or add a
#'  parametric distribution curve to an existing plot.
#' @param breaks Integer defining number of breaks when generating histogram
#'  (passed to \code{\link{hist}}).
#' @param lpar A list of plotting parameters controlling appearance of the
#'  density curve line (if present - passed to \code{\link{lines}}).
#' @param ... Additional parameters passed to \code{plot}, including those
#'  affecting histogram appearance.
#' @return None.
#' @examples
#'   # Fit and plot log-normal and Weibull models
#'   data(BCI_speed_data)
#'   agoutiData <- subset(BCI_speed_data, species=="agouti")
#'   lmod <- sbm(speed~1, agoutiData, pdf="lnorm")
#'   wmod <- sbm(speed~1, agoutiData, pdf="weibull")
#'   hist(lmod, breaks = 40)
#'   hist(wmod, add=TRUE, lpar=list(col="blue"))
#' @export
#'
hist.sbm <- function(x, log=TRUE, add=FALSE, breaks=20, lpar=list(col="red"),
                     ...){
  dots <- list(...)
  allvars <- all.vars(x$formula)
  dat <- get(allvars[1], x$data)
  if(log) dat <- log10(dat)
  h <- hist(dat, breaks=breaks, plot=FALSE)
  frq <- if("freq" %in% names(dots)) dots$freq else TRUE

  if(x$pdf != "none" & length(allvars) == 1){
    cfs <- bbmle::coef(x$model)
    sq <- seq(min(h$breaks), max(h$breaks), len=256)
    if(log) sq <- exp(sq)
    den <- switch(x$pdf,
                  gamma = dsbgamma(sq, cfs[1], cfs[2], xlog=log),
                  lnorm = dsblnorm(sq, cfs[1], cfs[2], xlog=log),
                  weibull = dsbweibull(sq, cfs[1], cfs[2], xlog=log))
    if(log) sq <- log(sq)

    if(frq){
      bin <- diff(h$breaks[1:2])
      den <- den * length(dat) * bin
    }

    mx <- max(den)
  } else mx <- 0

  if(!add){
    ppar <- c(list(x=h), dots)
    pargs <- names(ppar)
    if(!"main" %in% pargs) ppar <- c(ppar, main="")
    if(!"xlab" %in% pargs){
      xl <- allvars[1]
      if(log==TRUE) xl <- paste0("log10(", xl, ")")
      ppar <- c(ppar, xlab=xl)
    }
    if(!"ylim" %in% pargs){
      d <- if(frq) h$counts else h$density
      lim <- range(c(0, mx, d))
      ppar <- c(ppar, list(ylim=lim))
    }
    do.call(plot, ppar)
  }

  if(x$pdf != "none" & length(allvars) == 1){
    do.call(lines, c(list(x=sq, y=den), lpar))
  }
}
