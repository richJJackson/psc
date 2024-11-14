#' A generic function for cleaning data ready for analysis
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param id to specify which observations in the data cohort should be evaluated.  Defualts to 'NULL' i.e all observations
#' @param trt used to specify multiple treatment effects. Defaults to NULL
#' @return datComb returns a list containing objects which detial the components of both the
#'   Counter Factual Model (CFM) and the Data Cohort (DC) the required exported components
#'   of the model and a cleaned data cohort.
#' @examples
#' library(psc)
#' library(survival)
#' data("surv.mod")
#' data("data")
#' dc <- dataComb(surv.mod,data)
#' @export
dataComb <- function(CFM,DC,id=NULL,trt=NULL){
  UseMethod("dataComb")
}


