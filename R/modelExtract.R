#' A generic function for extracting model information
#' @param CFM a model object supplied to pscfit
modelExtract <- function(CFM){
  UseMethod("modelExtract")
}

