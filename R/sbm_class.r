#' Size biased model class.
#'
#' An S3 class describing parametric or non-parametric size biased models.
#' Typically contains elements:
#'  \itemize{
#'   \item{estimate: }{A dataframe with fields \code{est}, \code{se}, \code{lcl},
#'    and \code{ucl} (estimate, standard error, upper and lower confidence
#'    limits), potentially with multiple rows if the model has covariates.}
#'   \item{model: }{A model fit of class \code{mle2}}.
#'   \item{pdf: }{A text value recording the probability distribution of the model.}
#'   \item{formula: }{A two-sided formula recording the response variable and
#'    any covariates.}
#'   \item{data: }{A dataframe holding the response variable and any covariates.}
#'  }
#'
#' @export
setClass("sbm", representation("list"))
