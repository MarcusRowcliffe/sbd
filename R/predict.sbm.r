#' Predict estimates
#'
#' Generates predicted underlying averages from a size biased model,
#' given a set of covariates if these are used in the model.
#'
#' @param object A size biased model fit of class \code{\link{sbm}}.
#' @param newdata A dataframe of covariate values with fields matching
#'  covariates used in \code{object}.
#' @param reps Integer giving the number of random draws for variance estimation.
#' @param ... Additional arguments (unused).
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
#'   nd <- data.frame(mass = c(1, 10, 100))
#'   predict(lmod_mass, nd)
#' @export
#'
predict.sbm <- function(object, newdata=NULL, reps=999, ...){

  if(object$pdf == "none"){
    hmod <- hmean(dplyr::pull(object$data))
    outp <- data.frame(est = hmod$mean,
                       se = hmod$se,
                       lcl = hmod$mean - 1.96 * hmod$se,
                       ucl = hmod$mean + 1.96 * hmod$se)

  } else{
    newdata <- make_newdata(object$formula, object$data, newdata)
    newdata$lmean <- 0
    if(inherits(newdata, "list")) newdata <- as.data.frame(newdata)
    cfs <- object$model@coef
    scfs <- MASS::mvrnorm(reps, cfs, object$model@vcov)
    i <- grep("lmean.", colnames(scfs))
    scfs <- scfs[,i]
    cfs <- cfs[i]
    ff <- formula(strsplit(object$model@formula, ": ")[[1]][2])
    m <- model.frame(ff, newdata)
    mat <- model.matrix(ff, m)
    res <- exp(mat %*% t(scfs))
    outp <- data.frame(newdata[, -ncol(newdata)],
                est=exp(mat %*% matrix(cfs, ncol=1)),
                se=apply(res, 1, sd),
                lcl=apply(res, 1, quantile, 0.025),
                ucl=apply(res, 1, quantile, 0.975))
    names(outp)[1:(ncol(newdata))-1] <- names(newdata)[-ncol(newdata)]
  }
  outp
}
