
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
