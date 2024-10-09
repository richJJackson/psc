#' A generic function for cleaning data ready for analysis
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param id to specify which observations in the data cohort should be evaluated.  Defualts to 'NULL' i.e all observations
#' @param trt used to specify multiple treatment effects. Defaults to NULL
#' @export
dataComb <- function(CFM,DC,id=NULL,trt=NULL){
  UseMethod("dataComb")
}


