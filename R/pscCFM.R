#' A function which provides basic summary information of a matrix x
#'
#' This function creates and object containaing all information relating to a
#' model which may be used for counter-factual inference.  This includes the
#' model parameters, the intended setting for it's, details of validation and
#' references for further details.
#'
#'
#' @param cfm a model object
#' @param covnm an optinal character vector suppling covariate names to the terms
#'     included in the model
#' @param setting and object from 'cfmSett' detailing the setting of the model.
#' @param valid an object from cfmValid which details validation of the model
#' @param citation a character giving a reference (if applicable),  defaults to NULL (e.g no citation)
#' @details Categorical/Character data are summarised by a table and Continuous
#'     data are summarised as median (IQR)
#' @export
pscCFM <- function(cfm,covnm=NULL,setting=NULL,valid=NULL,citation=NULL){

  ##################
  ##### Set-up object structure
  ret <- list()
  ret$class <- class(cfm)
  ret$setting <- list()
  ret$model <- list()
  ret$valid <- list()

  class(ret) <- "cfm"

  ##################
  #### Setting - Details of where data are taken from and a summary of the data
  ##################

  ### Setting - PICO
  if(is.null(setting)) setting <- cfmSett()

  ### Setting - Data Summary
  dset <- cfm$data$m

  ### Summaring the dataset
  cfm.data <- lapply(dset[,-c(1,ncol(dset))],dataSumm)
  cfm.data$modnames <- names(cfm.data)
  cfm.data$varnames <- names(cfm.data)

  if(!is.null(covnm)){
    if(length(covnm)!=length(cfm.data$modnames)) stop("Length of covnm much match terms in model")
    cfm.data$varnames <- covnm
  }

  ## Adding in 'pico' and data summary to object
  ret$setting$pico <- setting
  ret$setting$data <- cfm.data

  ##################
  ### Model
  ##################
  modelEx <- modelExtract(cfm)
  modelEx <- modelEx[-which(names(modelEx)=="model.frame")]
  ret$model <- modelEx

  ##################
  #### Validation
  ##################
  if(is.null(valid)){
    valid <- cfmValid(cfm)
  }

  ## Adding in 'pico' and data summary to object
  ret$valid <- valid

  ##################
  #### Citation
  ##################
  if(is.null(citation)){
    citation <- "No Citation provided"
  }

  ret$citation <- citation

  ## returning results
  return(ret)
}
