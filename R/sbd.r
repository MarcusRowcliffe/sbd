library(bbmle)
library(MASS)

setClass("sbm", representation("list"))

#Harmonic mean and standard error
hmean <- function(x){
  mn <- 1/mean(1/x)
  se <- mn^2 * sqrt(var(1/x)/length(x))
  list(mean=mn, se=se)
}

#Size biased log normal probability density
dsblnorm = function(x, lmean, lsig, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dlnorm(x, lmean-exp(lsig)^2/2, exp(lsig)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  

#Size biased gamma probability density
dsbgamma = function(x, lmean, lrate, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dgamma(x, exp(lmean)*exp(lrate), exp(lrate)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  

#Size biased Weibull probability density
dsbweibull = function(x, lmean, lshape, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dweibull(x, exp(lshape), exp(lmean)/gamma(1+1/exp(lshape))) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  

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

# Makes a table of new data for prediction, based on original data
# if newdata=NULL (in which case mean numeric variables and all category 
# combos created), otherwise returns newdata with non-numeric variables
# as factors
make_newdata <- function(formula, data, newdata=NULL){
  covars <- tail(all.vars(formula), -1)
  if(!all(covars %in% names(data)))
    stop("Can't find all formula variables in data")
  
  data <- as.data.frame(unclass(data), stringsAsFactors=T)
  
  if(is.null(newdata) & length(covars)>0)
    newdata <- data %>% 
      dplyr::select(dplyr::all_of(covars)) %>%
      lapply(function(x)
        if(is.numeric(x)) mean(x, na.rm=T) else levels(x)) %>%
      expand.grid()
  
  if(!is.null(newdata)){
    if(!all(covars %in% names(newdata)))
      stop("Can't find all formula variables in newdata")
    for(v in covars){
      if(!is.numeric(newdata[, v]))
        newdata[, v] <- factor(newdata[, v], levels=levels(data[, v]))
    }
  }
  newdata
}

#Predict average speed
#INPUT
# mod: a size biased model created using sbm
# newdata: a dataframe containing covarariate values at which to predict speed
# reps: number of random replicates over which to calculate SE
predict.sbm <- function(mod, newdata=NULL, reps=1000){
  if(mod$pdf == "none")
    stop("You can't predict from a non-parametric fit")
  
  newdata <- make_newdata(mod$formula, mod$data, newdata)
  newdata$lmean <- 0
  if(class(newdata) == "list") newdata <- as.data.frame(newdata)
  cfs <- mod$model@coef
  scfs <- MASS::mvrnorm(reps, cfs, mod$model@vcov)
  i <- grep("lmean.", colnames(scfs))
  scfs <- scfs[,i]
  cfs <- cfs[i]
  ff <- formula(strsplit(mod$model@formula, ": ")[[1]][2])
  m <- model.frame(ff, newdata)
  mat <- model.matrix(ff, m)
  res <- exp(mat %*% t(scfs))
  outp <- data.frame(newdata[, -ncol(newdata)], 
              est=exp(mat %*% matrix(cfs, ncol=1)),
              se=apply(res, 1, sd),
              lcl=apply(res, 1, quantile, 0.025),
              ucl=apply(res, 1, quantile, 0.975))
  names(outp)[1:(ncol(newdata))-1] <- names(newdata)[-ncol(newdata)]
  outp
}

#Fits all three size biased options
#INPUT 
# As for sbm
#OUTPUT
# A list containing:
#  models: a list containing the three fitted sbm models
#  AICtab: a table of AIC and deltaAIC values for each model
sbm3 <- function(formula, data, reps=1000){
  mods <- list(sbm(formula, data, "lnorm"),
               sbm(formula, data, "gamma"),
               sbm(formula, data, "weibull")
  )
  names(mods) <- c("lnorm", "gamma", "weibull")
  AICs <- unlist(lapply(mods, AIC))
  i <- order(AICs)
  tab <- data.frame(AIC=AICs, dAIC=AICs-min(AICs))[i, ]
  rownames(tab) <- names(mods)[i]
  list(models=mods, AICtab=tab)
}

#Extract AIC from a size biased model
AIC.sbm <- function(mod){
  if(mod$pdf == "none")
    NULL else
      AIC(mod$model)
}

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
