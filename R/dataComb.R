#' A generic function for cleaning data ready for analysis
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
dataComb <- function(CFM,DC,id=NULL,trt=NULL){
  UseMethod("dataComb")
}


