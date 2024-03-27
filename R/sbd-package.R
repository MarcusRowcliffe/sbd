#' Size Biased Distributions
#'
#' Fitting and plotting parametric or non-parametric size-biased
#' non-negative distributions, with optional covariates in the case of
#' parametric. Supports three parametric options, log-normal, Weibull, and gamma.
#'
#' @details
#'   The core function is \code{\link{sbm}}, which fits a model to
#'   non-negative observations to estimate the average of the underlying
#'   distribution assuming that the probability of making an observation is
#'   proportional to the size of that observation. The default gives a
#'   non-parametric fit (the harmonic mean), and three parametric options are
#'   also available: log-normal, Weibull, and gamma. Covariates can be included
#'   in parametric models. The output is a list of class \code{sbm}, which has
#'   methods \code{plot}, \code{predict}, \code{summary}, and \code{AIC}. The
#'   functions were developed to support the analysis of speed observations from
#'   camera trap data described by Rowcliffe et al. (2016).
#' @references
#'   Patil, G. P. 2002 Weighted distributions. Pp. 2369–2377 in A.H. El-Shaarawi,
#'   W. W. Piegorsch, eds. Encycolpedia of Environmetrics. Wiley, Chichester.
#'
#'   Rowcliffe, J.M., Jansen, P.A., Kays, R., Kranstauber, B., and
#'   Carbone, C. (2016). Wildlife speed cameras: measuring animal travel speed
#'   and day range using camera traps. Remote Sensing in Ecology and
#'   Conservation 2, 84-94.
#' @name sbd
"_PACKAGE"
