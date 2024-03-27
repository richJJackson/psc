### Data linear predictors


data.comb.fpm <- function(model.extract,DC){

	### removing response and weights
	mf <- model.extract$model.frame
	term.nm <- names(mf)
	term.nm <- term.nm[-c(1,length(term.nm))];term.nm

	### ERROR CHECK: Selecting data from DC
	data_unavail_id  <- which(!term.nm%in%names(DC))
	data_unavail <- term.nm[data_unavail_id]


	### Creating model matrix (adding resposne to covariates)
	DC <- data.frame(cbind(0,DC))
	names(DC)[1] <- names(mf)[1]
	DC[,1] <- Surv(DC$time,DC$cen)

	### Finding missing data
	DC2 <- DC[,which(names(DC)%in%c(term.nm,"time","cen"))]
	miss.id <- unique(which(is.na(DC2),arr.ind=T)[,1])

	if(length(miss.id)>0) {
		DC <- DC[-miss.id,]
	}

	### Estimating linear predictor
	dc_mm <- model.matrix(model.extract$formula,data=DC)[,-1]
	out <- data.frame("time"=DC$time,"cen"=DC$cen)
	nrow(out)

	ret <- list("cov"=dc_mm,"outcome"=out)
	ret
}




data.comb.glm <- function(model.extract,DC){

	### removing response and weights
	mf <- model.extract$model.frame
	term.nm <- names(mf)
	out.nm <- term.nm[1];out.nm
	term.nm <- term.nm[-c(1,length(term.nm))]

	### ERROR CHECK: Selecting data from DC
	data_unavail_id  <- which(!term.nm%in%names(DC))
	data_unavail <- term.nm[data_unavail_id]

	### Estimating linear predictor
	dc_mm <- model.matrix(model.extract$formula,data=DC)

	out <- data.frame(out.nm=DC[,which(names(DC)==out.nm)])
	names(out) <- out.nm
	ret <- list("cov"=dc_mm,"outcome"=out)
	ret
}



