### Estimation

#### Functions
modp <- function(x){
	x*(sign(x)+1)/2
}

acc <-  function(old,new){
	ret <- FALSE
	r <- runif(1)
	e <-exp(old-new)
	try(if(e>r) ret <- TRUE)
	ret
}






### Assumed data in 'data' match fpm output

mcmc.fpm <- function(nsim,start,model.extract,DC.clean){

	cov_co <- model.extract$cov_co;cov_co
	haz_co <- model.extract$haz_co;haz_co
	sig <- model.extract$sig
	lam <- model.extract$lam
	kn <- model.extract$kn
	time <- DC.clean$out$time;time
	cen <- DC.clean$out$cen
	cov <- DC.clean$cov

	est <- c(haz_co,cov_co)

	####### Bayesian Estimation
	beta <- start
	parm <- matrix(NA,nsim,length(est)+2)
	parm[1,]<- c(est,beta,NA)

	## Progress Bar
	pb <- txtProgressBar(min = 0, max = nsim, style = 3)


	for(n in 2:nsim){

		## progress bar
		setTxtProgressBar(pb, n)

		### Drawing Samples
		cand <- rmvnorm(1,est,sig)
		cand.beta <- rnorm(1,0,1)
		parm[n,] <- c(cand,cand.beta,NA)

		### partitioning covariates into baseline hazard and coefficients
		cand.haz <- cand[1:length(haz_co)]
		cand.cov <- cand[-c(1:length(haz_co))]

		### cadidate log Hazard Ratios
		beta.old <- parm[n-1,(length(cand)+1)];beta.old
		beta.new <- parm[n,(length(cand)+1)];beta.new

		### Prior contribution
		pr.cand <- -dmvnorm(cand,est,sig,log=T)
		pr.old <- log(dnorm(beta.old,0,1000))
		pr.new <- log(dnorm(beta.new,0,1000))

		### Likelihood evaluation
		l.old <- lik.fpm(beta.old,cand.cov,cand.haz,model.extract,DC.clean) + pr.old + pr.cand
		l.new <- lik.fpm(beta.new,cand.cov,cand.haz,model.extract,DC.clean)  + pr.new + pr.cand

		### Accept/Reject
		parm[n,ncol(parm)] <- l.new
		if(!acc(l.old,l.new)) {
			parm[n,(length(cand)+1)] <- beta.old
			parm[n,ncol(parm)] <- l.old
			}

	}

	parm
}





mcmc.ef <- function(nsim,start,model.extract,DC.clean){

	cov_co <- model.extract$cov_co;cov_co
	sig <- model.extract$sig

	est <- c(cov_co)

	####### Bayesian Estimation
	beta <- start;
	parm <- matrix(NA,nsim,length(est)+2)
	parm[1,]<- c(est,beta,NA)

	## Progress Bar
	pb <- txtProgressBar(min = 0, max = nsim, style = 3)

	for(n in 2:nsim){

		## progress bar
		setTxtProgressBar(pb, n)

		### Drawing Samples
		cand <- rmvnorm(1,est,sig);cand
		cand.beta <- rnorm(1,0,1)
		parm[n,] <- c(cand,cand.beta,NA)

		### cadidate log Hazard Ratios
		beta.old <- parm[n-1,(length(cand)+1)];beta.old
		beta.new <- parm[n,(length(cand)+1)];beta.new

		### Prior contribution
		pr.cand <- -dmvnorm(cand,est,sig,log=T);pr.cand
		pr.old <- log(dnorm(beta.old,0,1000));pr.old
		pr.new <- log(dnorm(beta.new,0,1000));pr.new

		### Likelihood evaluation
		l.old <- lik.ef(beta.old,t(cand),DC.clean,model.extract$family) + pr.old + pr.cand
		l.new <- lik.ef(beta.new,t(cand),DC.clean,model.extract$family) + pr.old + pr.cand

		### Accept/Reject
		parm[n,ncol(parm)] <- l.new
		if(!acc(l.old,l.new)) {
			parm[n,(length(cand)+1)] <- beta.old
			parm[n,ncol(parm)] <- l.old
			}

	}

	parm
}

mcmc.logit <- function(nsim,start,model.extract,DC.clean){

	cov_co <- model.extract$cov_co;cov_co
	sig <- model.extract$sig

	est <- c(cov_co)

	####### Bayesian Estimation
	beta <- start;
	parm <- matrix(NA,nsim,length(est)+2)
	parm[1,]<- c(est,beta,NA)

	## Progress Bar
	pb <- txtProgressBar(min = 0, max = nsim, style = 3)

	for(n in 2:nsim){

		## progress bar
		setTxtProgressBar(pb, n)

		### Drawing Samples
		cand <- rmvnorm(1,est,sig);cand
		cand.beta <- rnorm(1,0,1)
		parm[n,] <- c(cand,cand.beta,NA)

		### cadidate log Hazard Ratios
		beta.old <- parm[n-1,(length(cand)+1)];beta.old
		beta.new <- parm[n,(length(cand)+1)];beta.new

		### Prior contribution
		pr.cand <- -dmvnorm(cand,est,sig,log=T);pr.cand
		pr.old <- log(dnorm(beta.old,0,1000));pr.old
		pr.new <- log(dnorm(beta.new,0,1000));pr.new

		### Likelihood evaluation
		l.old <- lik.logit(beta.old,t(cand),DC.clean) + pr.old + pr.cand
		l.new <- lik.logit(beta.new,t(cand),DC.clean) + pr.old + pr.cand

		### Accept/Reject
		parm[n,ncol(parm)] <- l.new
		if(!acc(l.old,l.new)) {
			parm[n,(length(cand)+1)] <- beta.old
			parm[n,ncol(parm)] <- l.old
			}

	}

	parm
}







