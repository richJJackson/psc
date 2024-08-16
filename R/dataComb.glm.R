#' Fucntion for cleaning the data of a model with class 'flexsurvreg'
#'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param id a vector specifiying whether a subset of the dataset shoudl be selcted.
#'   Defaults to 'NULL' e.g. all data points included
#' @return a list containing objects which specifiy the required exported components
#'   of the model and a cleaned data cohort.
#' @export
dataComb.glm <- function(CFM,DC,id=NULL,trt=NULL){

  ### removing response and weights
  model_extract <- modelExtract(CFM);model_extract
  mf <- model_extract$model.frame
  term.nm <- names(mf)
  out.nm <- term.nm[1]
  length(term.nm)

  term.nm <- term.nm[-1];term.nm
  data_unavail_id <- which(!term.nm %in% names(DC))
  data_unavail <- term.nm[data_unavail_id]
  if (length(data_unavail_id) != 0)
    stop(paste("Covariate '", data_unavail, "' is included in the model but not the dataset",
               sep = ""))
  out.id <- which(names(DC) %in% c(out.nm))
  if (length(out.id) != 1)
    stop(paste("Please ensure covariates for the outcom labelled",
               out.nm, "is included"))


  ### Adding treatment variable (if not null)
  if(!is.null(trt)) {
    if("trt"%in%names(DC)){
      DC <- DC[,-which(names(DC)=="trt")]
    }
    DC <- cbind(DC,trt)
    term.nm <- c(term.nm,"trt")
  }

  ### Finding missing data
  DC2 <- DC[, which(names(DC) %in% c(term.nm, out.nm))]
  miss.id <- unique(which(is.na(DC2), arr.ind = T)[, 1])

  if (length(miss.id) > 0) {
    DC <- DC[-miss.id, ]
    warning(paste(length(miss.id), "rows removed due to missing data in dataset"))
  }

  ## Creating the model matrix
  dc_mm <- model.matrix(model_extract$formula, data = DC)

  if(!is.null(trt)) dc_mm <- cbind(dc_mm,"trt"=DC$trt)

  out <- data.frame(out.nm = DC[, which(names(DC) == out.nm)])
  names(out) <- out.nm

  if(!is.null(id)){
    dc_mm <- dc_mm[id,]
    out <- out[id,]
  }

  ret <- list("model_extract"=model_extract,"cov"=dc_mm,"outcome"=out)
  ret

  }
