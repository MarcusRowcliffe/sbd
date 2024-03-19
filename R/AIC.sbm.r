
#Extract AIC from a size biased model
AIC.sbm <- function(mod){
  if(mod$pdf == "none")
    NULL else
      AIC(mod$model)
}
