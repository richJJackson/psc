
### Likelihood Function
lik.fpm <- function(beta,cov_co,haz_co,model.extract,DC.clean){


	lam <- model.extract$lam
	kn <- model.extract$kn
	time <- DC.clean$out$time;time
	cen <- DC.clean$out$cen
	cov <- DC.clean$cov

	logt <- log(time)
	lp <- cov%*%cov_co
	k <- model.extract$k

	z <- NULL
	z_h <- NULL
	### basis functions
	for(i in 1:k){
		zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
		z <- cbind(z,zt)

		zt_h <- (modp(logt-kn[(i+1)])^2 - lam[(i+1)]*modp(logt-kn[1])^2 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^2)
		z_h <- cbind(z_h,zt_h)

	}


	H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
	h0 <- (H0/time)*(haz_co[2]+3*z_h%*%haz_co[3:(2+k)])

	H<- H0*exp(lp+beta)
	h<- h0*exp(lp+beta)
	S <- exp(-H)
	f <- S*h

	l <- sum(cen*log(f+1e-16) + (1-cen)*log(S+1e-16))
	-l

}




### Likelihood Function - logistic regression
lik.logit <- function(beta,cov_co,DC.clean){

	event <- DC.clean$out;event
	cov <- DC.clean$cov;cov

	lp <- cov%*%cov_co + beta
	llp <- inv.logit(lp)

	l <- sum(event*lp-log(1+exp(lp)))
	-l

}




### Likelihood Function
lik.ef <- function(beta,cov_co,DC.clean,family){

	## Getting data
	event <- DC.clean$out;event
	cov <- DC.clean$cov;cov
	fam <- enrich(family)

	### defining likelihood
	lp <- cov%*%cov_co + beta
	mu <- fam$linkinv(lp)
	theta <- fam$theta(mu)
	btheta <- fam$bfun(theta)

	## returning result
	- sum(event*theta-btheta)


}
