#' A function to store setting information for a cfm model
#'
#' This function collects infomation ont he setting of a model for the
#' definition of a Counter Factual Model (CFM) object.  Setting information is
#' provided in the PICO format.  Intended to be used in conjuction with the
#' pscCRM.R function
#'
#' @param P Character; patient group
#' @param I Character; intervention group - perhaps left unspecified
#' @param C Character; comparitor group
#' @param O Character; outcome definition
#' @details A function for containg basic information pertaining to the setting
#' of a model in PICO format
#' @export
cfmSett <- function(P=NULL,I=NULL,C=NULL,O=NULL){

  if(is.null(P)){
    P <- "No Patient Populations defined"
  }

  if(is.null(I)){
    I <- "No Intervention defined"
  }

  if(is.null(C)){
    C <- "No Comparitor defined"
  }

  if(is.null(O)){
    O <- "No Outcome defined"
  }

  ret <- list("P"=P,"I"=I,"C"=C,"O"=O)
  ret
}
