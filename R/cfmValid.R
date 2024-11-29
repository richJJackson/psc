#' Validation of a flexsurvreg model
#' This function intends to perform basic validation of a survival model of class
#' 'flexsurvreg'.  As a default internal validation will be performed by estimating
#' basic measures of discrimination and calibration. If provided, measures will
#' be estimated on external data. This is intended to be used in conjunction
#' with the pscCRM.R function#' A generic function for extracting model information
#' @param cfm a model of class 'flexsurvreg'
#' @param exData an external dataset (for external validation)
#' @return a list of validation procedures for measures of discrimination and
#'     calibration.  Discrimination performed by creating a variable with risk
#'     categories based on the 15th, 50th and 85th percentile of the linear
#'     predictor.  Discrimation estimated provided in terms of kaplan merie
#'     estimates and hazard ratios.
#'
#'     Calibration performed by extracting the concordance statistic and by
#'     regression the linear predictor against the outcome.
#'
#'     Validation is internal unless an external dataset is provided.
cfmValid <- function(cfm, exData){
  UseMethod("cfmValid")
}

