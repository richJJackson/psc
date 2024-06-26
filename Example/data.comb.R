#' Functions for cleaning a dataset to match the output of a CFM
#'
#' @param model.extract details of model extract from model extract
#' @param DC A dataset to compare against the CFM
#' @return A cleaned dataset and outcome
#'
data.comb.fpm <- function(model.extract,DC,id=NULL){

	### removing response and weights
	mf <- model.extract$model.frame
	term.nm <- names(mf)
	term.nm <- term.nm[-c(1,length(term.nm))];term.nm


  ### ERROR CHECK: Selecting data from DC
	data_unavail_id  <- which(!term.nm%in%names(DC))
	data_unavail <- term.nm[data_unavail_id]
	if(length(data_unavail_id)!=0) stop(paste("Covariate '",data_unavail,"' is included in the model but not the dataset",sep=""))

	### Making sure 'time' and 'cen'
  out.id <- which(names(DC)%in%c("time","cen"))
	if(length(out.id)!=2) stop("Please ensure covariates to define the outcome are labeled as 'time' and 'cen'")

	### Creating model matrix (adding resposne to covariates)
	DC <- data.frame(cbind(0,DC))
	names(DC)[1] <- names(mf)[1]
	DC[,1] <- Surv(DC$time,DC$cen)

	### Finding missing data
	DC2 <- DC[,which(names(DC)%in%c(term.nm,"time","cen"))]
	miss.id <- unique(which(is.na(DC2),arr.ind=T)[,1])

	if(length(miss.id)>0) {
		DC <- DC[-miss.id,]
    warning(paste(length(miss.id),"rows removed due to missing data in dataset"))
	}

	### Estimating linear predictor
	dc_mm <- model.matrix(model.extract$formula,data=DC)[,-1]
  out <- data.frame("time"=DC$time,"cen"=DC$cen)

	if(!is.null(id)){
	  dc_mm <- dc_mm[id,]
	  out <- out[id,]
	}


	ret <- list("cov"=dc_mm,"outcome"=out)
  ret
}




data.comb.glm <- function (model.extract, DC) {
  mf <- model.extract$model.frame
  term.nm <- names(mf)
  out.nm <- term.nm[1]
  length(term.nm)

  term.nm <- term.nm[-1]
  data_unavail_id <- which(!term.nm %in% names(DC))
  data_unavail <- term.nm[data_unavail_id]
  if (length(data_unavail_id) != 0)
    stop(paste("Covariate '", data_unavail, "' is included in the model but not the dataset",
               sep = ""))
  out.id <- which(names(DC) %in% c(out.nm))
  if (length(out.id) != 1)
    stop(paste("Please ensure covariates for the outcom labelled",
               out.nm, "is included"))
  DC2 <- DC[, which(names(DC) %in% c(term.nm, out.nm))]
  miss.id <- unique(which(is.na(DC2), arr.ind = T)[, 1])


  if (length(miss.id) > 0) {
    DC <- DC[-miss.id, ]
    warning(paste(length(miss.id), "rows removed due to missing data in dataset"))
  }
  dc_mm <- model.matrix(model.extract$formula, data = DC)
  out <- data.frame(out.nm = DC[, which(names(DC) == out.nm)])
  names(out) <- out.nm

  if(!is.null(id)){
    dc_mm <- dc_mm[id,]
    out <- out[id,]
  }

  ret <- list(cov = dc_mm, outcome = out)
  ret
}


