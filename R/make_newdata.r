#' Make new data for prediction
#'
#' Creates or converts a dataframe of covariates to pass to
#' \code{\link{predict.sbm}}, based on the \code{data} used in the model to be
#' used for prediction.
#'
#' @param formula A two-sided formula of the form
#'  \code{response ~ covariate + ...}
#' @param data A dataframe containing the fields named in formula.
#' @param newdata A dataframe of covariate values with fields matching variables
#'  in \code{formula}.
#' @return A dataframe of covariate values.
#' @details When \code{newdata} is missing, a new dataframe is created with
#'  numeric variables held at their mean values, and all combinations of factors.
#'  When a \code{newdata} dataframe is provided it is checked for compatibility
#'  with \code{formula} variables, and character variables are converted to factors,
#'  but otherwise passed unchanged.
#' @examples
#'   data(BCI_speed_data)
#'   make_newdata(speed ~ species, BCI_speed_data)
#' @export
#'
make_newdata <- function(formula, data, newdata=NULL){
  covars <- tail(all.vars(formula), -1)
  if(!all(covars %in% names(data)))
    stop("Can't find all formula variables in data")

  data <- as.data.frame(unclass(data), stringsAsFactors=T)
  if(is.null(newdata)){
     if(length(covars)>0)
       newdata <- data %>%
         dplyr::select(dplyr::all_of(covars)) %>%
         lapply(function(x)
           if(is.numeric(x)) mean(x, na.rm=T) else levels(x)) %>%
         expand.grid()
  } else{
    if(!all(covars %in% names(newdata)))
      stop("Can't find all formula variables in newdata")
    for(v in covars){
      if(!is.numeric(pull(newdata[, v])))
        newdata[, v] <- factor(newdata[, v], levels=levels(data[, v]))
    }
  }
  newdata
}
