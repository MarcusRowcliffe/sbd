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
#' @param trace Logical defining whether to show diagnostic information when
#'  fitting parametric distributions (passed to \code{\link[bbmle]{mle2}}).
#' @return A list of class \code{sbm} with methods \code{\link{summary.sbm}},
#'  \code{\link{predict.sbm}}, \code{\link{hist.sbm}}, and
#'  \code{\link{AIC.sbm}}. The list has elements:
#'  \itemize{
#'   \item{"estimate"}{A dataframe of estimated averages, their standard
#'    errors and 95% confidence limits; returns a single row for models without
#'    covariates; or with covariates, one estimate per unique combination of
#'    factor covariate levels, with any quantitative covariates held at their
#'    mean values.}
#'   \item{"data"}{A dataframe containing the data used to fit the model.}
#'   \item{"model"}{A model object of class \code{\link[bbmle]{mle2}.}}
#'   \item{"formula"}{The formula supplied to the function call.}
#'   \item{"pdf"}{Character string recording the probability density function
#'    used to fit the model.}
#' }
#' @details Response values must be strictly positive. To fit a distribution
#'  without covariates use 1 on the right hand side of the formula. When
#'  pdf = "none", the harmonic mean and it's standard error are calculated,
#'  and no covariates can be used.
#' @examples
#'   data(BCI_speed_data)
#'   agoutiData <- subset(BCI_speed_data, species=="agouti")
#'
#'   # harmonic mean estimate for agouti
#'   hmod <- sbm(speed~1, agoutiData)
#'   lmod <- sbm(speed~1, agoutiData, pdf="lnorm")
#'
#'   # lognormal estimate with or without a covariates
#'   lmod_mass <- sbm(speed~mass, BCI_speed_data, pdf="lnorm")
#'   lmod_spp <- sbm(speed~species, BCI_speed_data, pdf="lnorm")
#'
#'   # inspect estimates
#'   hmod$estimate
#'   lmod$estimate
#'   lmod_mass$estimate
#'   lmod_spp$estimate
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
    res <- list(estimate=est,
                model=NULL,
                pdf=dstrbn,
                formula=formula,
                data=dat) %>%
      new_sbm()
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
    model <- bbmle::mle2(f1, start=startpars, data=dat, method="L-BFGS-B",
                  lower=lwr, upper=upr, parameters=list(f2), trace=trace)

    res <- list(estimate=NULL,
                model=model,
                pdf=dstrbn,
                formula=formula,
                data=dat)
    class(res) <- "sbm"
    res$estimate <- predict(res)
  }
  res
}
