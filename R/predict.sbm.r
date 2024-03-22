
#Predict average speed
#INPUT
# mod: a size biased model created using sbm
# newdata: a dataframe containing covarariate values at which to predict speed
# reps: number of random replicates over which to calculate SE
#' Predict estimates
#'
#' Generates predicted underlying averages from a size biased model,
#' given a set of covariates if these are used in the model.
#'
#' @param mod A size biased model fit of class \code{\link{sbm}}.
#' @param newdata A dataframe of covariate values with fields matching
#'  covariates used in \code{mod}.
#' @param reps Integer giving the number of random draws for variance estimation.
#' @return A dataframe of predictions with fields \code{est} (estimated average),
#'  \code{se} (estimated standard error), and \code{lcl}, \code{ucl} (lower and
#'  upper 95 percent confidence limits).
#' @details When \code{newdata} is missing, \code{make_table} is used to
#'  generate a dataframe of covariates at which to predict, based on the
#'  model formula and covariate data.
#' @examples
#'   data(BCI_speed_data)
#'   agoutiData <- subset(BCI_speed_data, species=="agouti")
#'   lmod_mass <- sbm(speed~mass, agoutiData, pdf="lnorm")
#'   nd <- data.frame(mass = seq(1, 50, len=100))
#'   prdn <- predict(lmod_mass, nd)
#' @export
#'
predict.sbm <- function(mod, newdata=NULL, reps=1000){
  if(mod$pdf == "none")
    stop("You can't predict from a non-parametric fit")

  newdata <- make_newdata(mod$formula, mod$data, newdata)
  newdata$lmean <- 0
  if(inherits(newdata, "list")) newdata <- as.data.frame(newdata)
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
