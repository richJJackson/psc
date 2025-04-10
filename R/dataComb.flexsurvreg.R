#' Function for cleaning the data of a model with class 'flexsurvreg'
#'
#' The purpose of this function is to prepare the dataset and the counter-factual
#' model for estimation and is the first step pf the pscfit.R process. The output
#' is a complete-case dataset where the data names match the variables used in the CFM.
#'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param id a vector specifiying whether a subset of the dataset should be selected.
#'   Defaults to 'NULL' e.g. all data points included
#' @param trt An optional additional vector denoting treatment allocations for multiple treatment comparisons.  Defaults to 'NULL'
#' @return a list containing objects which specifiy the required exported components
#'   of the model and a cleaned data cohort.
#'  Attributes include \itemize{
#'  \item{'model.type' specifying the class of model to be used as the CFM }
#'  \item{'model_extract' sepcifying the model componets required for estimation}
#'  \item{'cov' a cleaned dataset of covariates}
#'  \item{'outcome' a cleaned dataset containing the outcomes}
#'  }
#' @export
dataComb.flexsurvreg <- function(CFM,DC,id=NULL,trt=NULL){

  ### removing response and weights
  model_extract <- modelExtract(CFM);model_extract
  mf <- model_extract$model.frame
  term.nm <- names(mf)
  term.nm <- term.nm[-c(1,length(term.nm))];term.nm

  ### ERROR CHECK: Selecting data from DC
  data_unavail_id  <- which(!term.nm%in%names(DC))
  data_unavail <- term.nm[data_unavail_id]
  if(length(data_unavail_id)!=0) stop(paste("Covariate '",data_unavail,"' is included in the model but not the dataset",sep=""))

  ### Making sure 'time' and 'cen' are present
  out.id <- which(names(DC)%in%c("time","cen"))
  if(length(out.id)!=2) stop("Please ensure covariates to define the outcome are labeled as 'time' and 'cen'")

  ## Creating model matrix (adding resposne to covariates)
  DC <- data.frame(cbind(0,DC))
  names(DC)[1] <- names(mf)[1]
  DC[,1] <- Surv(DC$time,DC$cen)

  ### Adding treatment variable (if not null)
  if(!is.null(trt)) {
    if("trt"%in%names(DC)){
      DC <- DC[,-which(names(DC)=="trt")]
    }
    term.nm <- c(term.nm,"trt")
  }

  ## Defining Outcome
  out.nm.trap <- which(names(DC)%in%c("time","cen"))
  if(length(out.nm.trap)!=2) stop("outcome covariates in data cohort should be named 'time' and 'cen'")
  out <- data.frame("time"=DC$time,"cen"=DC$cen)

  ### Matching data between DC and CFM
  DCcov <- data_match(mf,DC);DC[1:4,];DCcov[1:4,];out[1:4,];trt[1:4]

  #### Selecting subgroup (if 'id' is specified)
  if(!is.null(id)){
    DCcov <- DCcov[id,]
    out <- out[id,]
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
    out <- out[-miss.id,]
    trt <- trt[-miss.id]
    warning(paste(length(miss.id),"rows removed due to missing data in dataset"))
  }

  ### Creating model matrix based on new dataset
  DCM <- cbind(DCcov,out)
  dc_mm <- model.matrix(model_extract$formula,data=DCM)[,-1]

  ### Adding in 'trt' (if required)
  if(!is.null(trt)) dc_mm <- cbind(dc_mm,"trt"=trt)
  ret <- list("model.type"=class(CFM),"model_extract"=model_extract,"cov"=dc_mm,"outcome"=out)
  ret
}
