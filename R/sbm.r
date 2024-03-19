#' Fit a size biased model
#'
#' Fits a parametric or non-parametric size biased distribution model to a
#' positive response variable.
#'
#' @param formula A two-sided formula of the form response ~ covariate + ...
#' @param data A dataframe containing the fields named in formula.
#' @param pdf A text value naming the probability density function to use.
#' @param var.range The range of log variance within which to search when
#'  fitting parametric distributions.
#' @param trace Logical defining whether to show fit information when fitting
#'  parametric distributions (passed to \code{\link[bbmle]{mle2}}.
#' @return A list of class \code{\link{sbm}}.
#' @details Response values must be strictly positive. To fit a distribution
#'  without covariates use 1 on the right hand side of the formula. When
#'  pdf = "none", the harmonic mean and it's standard error are calculated,
#'  and no covariates can be used.
#' @examples
#'   data(data)
#'   hmean(data$speed)
#' @export
#'
sbm <- function(formula, data, pdf=c("none", "lnorm", "gamma", "weibull"),
                var.range=c(-4,4), trace=FALSE){
  dstrbn=match.arg(pdf)

  vars <- all.vars(formula)
  if(!all(vars %in% names(data)))
    stop("Can't find all formula variables in data")

  dat <- model.frame(formula, data)
  y <- model.response(dat)
  hmod <- hmean(y)

  if(dstrbn == "none"){
    if(length(vars) > 1)
      stop("You can't model covariates with a non-parametric fit")
    est <- data.frame(est = hmod$mean,
                      se = hmod$se,
                      lcl = hmod$mean - 1.96 * hmod$se,
                      ucl = hmod$mean + 1.96 * hmod$se)
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
    model <- mle2(f1, start=startpars, data=dat, method="L-BFGS-B",
                  lower=lwr, upper=upr, parameters=list(f2), trace=trace)

    res <- list(model=model, pdf=dstrbn, formula=formula, data=dat)
    class(res) <- "sbm"
    res$estimate <- predict(res)
  }
  res
}
