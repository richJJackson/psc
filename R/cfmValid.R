#' A generic function for extracting model information
#' @param CFM a model object to be validated
cfmValid <- function(CFM){
  UseMethod("cfmValid")
}

