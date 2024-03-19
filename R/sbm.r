
#Size biased model
#INPUT
# formula: a model formula with speed variable on the left and covariates (or 1) on the right
# data: a dataframe containing the speed variable and any covariates
# pdf: which (size biased) distribution to fit
sbm <- function(formula, data, pdf=c("none", "lnorm", "gamma", "weibull"),
                var.range=c(-4,4), trace=FALSE){
  dstrbn=match.arg(pdf)
  y <- model.response(model.frame(formula, data))
  hmod <- hmean(y)

  if(dstrbn == "none"){
    if(length(all.vars(formula)) > 1)
      stop("You can't model covariates with a non-parametric fit")
    est <- data.frame(est=hmod$mean, se=hmod$se,
                      lcl=hmod$mean - 1.96 * hmod$se,
                      ucl=hmod$mean + 1.96 * hmod$se)
    dat <- data.frame(y)
    names(dat) <- as.character(formula)[2]
    res <- list(estimate=est, model=NULL, pdf=dstrbn, formula=formula, data=dat)
    class(res) <- "sbm"
  } else{

    lmn <- log(hmod$mean)
    lv <- switch(dstrbn,
                  lnorm = log(sd(log(y))),
                  gamma = log(exp(lmn)/var(y)),
                  weibull = 0)
    startpars <- switch(dstrbn,
                        lnorm = list(lmean=lmn, lsig=lv),
                        gamma = list(lmean=lmn, lrate=lv),
                        weibull = list(lmean=lmn, lshape=lv))
    lwr <- switch(dstrbn,
                  lnorm = c(lsig=var.range[1]),
                  gamma = c(lrate=var.range[1]),
                  weibull = c(lshape=var.range[1]))
    upr <- switch(dstrbn,
                  lnorm = c(lsig=var.range[2]),
                  gamma = c(lrate=var.range[2]),
                  weibull = c(lshape=var.range[2]))
    f1 <- switch(dstrbn,
                 lnorm = as.formula(paste(as.character(formula)[2], "~ dsblnorm(lmean, lsig)")),
                 gamma = as.formula(paste(as.character(formula)[2], "~ dsbgamma(lmean, lrate)")),
                 weibull = as.formula(paste(as.character(formula)[2], "~ dsbweibull(lmean, lshape)"))
                 )
    f2 <- as.formula(paste("lmean ~", as.character(formula)[3]))
    model <- mle2(f1, start=startpars, data=data, method="L-BFGS-B",
                  lower=lwr, upper=upr, parameters=list(f2), trace=trace)

    res <- list(model=model, pdf=dstrbn, formula=formula, data=as.data.frame(model@data))
    class(res) <- "sbm"
    res$estimate <- predict(res)
  }
  res
}
