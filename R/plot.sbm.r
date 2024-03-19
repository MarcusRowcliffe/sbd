
#Plot a size biased model data and fitted distributions
#INPUT
# obj: a size biased model fitted with sbm
# log: whether to plot the distribution log scale
# lpar: plotting paramaters defining fitted line characteristics
# ppar: plot parameters passed to plot
# hpar: parameters passed histogram (not plotting parameters - pass these to ppar)
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
