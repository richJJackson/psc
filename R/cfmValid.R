#' Validation of a counter-factual model
#' This function intends to perform basic validation of a coutnerfactual model of
#' class 'glm' or 'flexsurvreg'.  As a default internal valudation will be
#' performed by estimating basic measures of discrimination and calibration.
#' If provided, measures will be estimated on external data. This is intended
#' to be used in conjunction with the pscCRM.R function#' A generic function for
#'  extracting model information
#' @param CFM a model object to be validated
#' @examples
#' library(psc)
#' cfmValid(psc::surv.mod)
cfmValid <- function(CFM){
  UseMethod("cfmValid")
}

