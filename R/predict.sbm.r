
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
