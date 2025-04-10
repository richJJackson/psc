#' Function for cleaning the data of a model with class 'flexsurvreg'
#'
#' The purpose of this function is to prepare the dataset and the counter-factual
#' model for estimation and is the first step pf the pscfit.R process. The output
#' is a complete-case dataset where the data names match the variables used in the CFM.
#'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param id to specify which observations in the data cohort should be evaluated.
#' Defualts to 'NULL' i.e all observations
#' @param trt used to specify multiple treatment effects. Defaults to NULL
#' @return a list containing objects which specifiy the required exported components
#'   of the model and a cleaned data cohort.  Attirbutes of the 'cleaned' object include:
#'  Attributes include \itemize{
#'  \item{'model.type' specifying the class of model to be used as the CFM }
#'  \item{'model_extract' sepcifying the model componets required for estimation}
#'  \item{'cov' a cleaned dataset of covariates}
#'  \item{'outcome' a cleaned dataset containing the outcomes}
#'  }
#' @export
dataComb.glm <- function(CFM,DC,id=NULL,trt=NULL){

  ### removing response and weights
  model_extract <- modelExtract(CFM);model_extract
  mf <- model_extract$model.frame
  term.nm <- names(mf)
  out.nm <- term.nm[1]
  term.nm <- term.nm[-1];term.nm

  ### ERROR CHECK: Selecting data from DC
  data_unavail_id <- which(!term.nm %in% names(DC))
  data_unavail <- term.nm[data_unavail_id]
  if (length(data_unavail_id) != 0)
    stop(paste("Covariate '", data_unavail, "' is included in the model but not the dataset",
               sep = ""))
  out.id <- which(names(DC) %in% c(out.nm))
  if (length(out.id) != 1)
    stop(paste("Please ensure covariates for the outcome labelled",
               out.nm, "is included"))


  ### Adding treatment variable (if not null)
  if(!is.null(trt)) {
    if("trt"%in%names(DC)){
      DC <- DC[,-which(names(DC)=="trt")]
    }
    term.nm <- c(term.nm,"trt")
  }

  ## Defining outcome
  out <- data.frame(out.nm = DC[, which(names(DC) == out.nm)])
  names(out) <- out.nm

  ### Matching data between DC and CFM
  DCcov <- data_match(mf,DC);DC[1:4,];DCcov[1:4,];out[1:4,];trt[1:4]

  #### Selecting subgroup (if 'id' is specified)
  if(!is.null(id)){
    DCcov <- DCcov[id,]
    out <- out[id]
    trt <- trt[id]
  }

  ### Removing missing data
  miss.cov <- which(is.na(DCcov),arr.ind=T)[,1]
  miss.out <- which(is.na(out),arr.ind=T)[,1]
  miss.trt <- which(is.na(trt))

  miss.id <- union(miss.cov,miss.out)
  miss.id <- union(miss.id,miss.trt)

  if(length(miss.id)>0) {
    DCcov <- DCcov[-miss.id,]
    out <- out[-miss.id]
    trt <- trt[-miss.id]
    warning(paste(length(miss.id),"rows removed due to missing data in dataset"))
  }

  ### Creating model matrix based on new dataset
  DCM <- cbind(DCcov,out)
  dc_mm <- model.matrix(model_extract$formula,data=DCM)


  ### Adding in 'trt' (if required)
  if(!is.null(trt)) dc_mm <- cbind(dc_mm,"trt"=DC$trt)

  ### returning results
  ret <- list("model.type"=class(CFM),"model_extract"=model_extract,"cov"=dc_mm,"outcome"=out)
  ret

  }
