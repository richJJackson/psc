#' Fucntion for estimating initial parameter values  'flexsurvreg'
#'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param nsim the number of MCMC simulations to run
#' @param start the stating value for
#' @details An MCMC routine for fitting a psc model
#' @return a matrix continig the draws form the posterior ditribution
#' @export
#'
pscEst.flexsurvreg <- function(CFM,DC_clean,nsim,start){

cov_co <- DC_clean$model_extract$cov_co;cov_co
haz_co <- DC_clean$model_extract$haz_co;haz_co
sig <- DC_clean$model_extract$sig
lam <- DC_clean$model_extract$lam
kn <- DC_clean$model_extract$kn
time <- DC_clean$out$time;time
cen <- DC_clean$out$cen
cov <- DC_clean$cov
est <- c(haz_co,cov_co);est


####### Bayesian Estimation
beta <- start
parm <- matrix(NA,nsim,length(est)+2)
parm[1,]<- c(est,beta,NA);parm[1,]

## Progress Bar
pb <- txtProgressBar(min = 0, max = nsim, style = 3)

for(n in 2:nsim){

  ## progress bar
  setTxtProgressBar(pb, n)

  ### Drawing Samples
  cand <- rmvnorm(1,est,sig)
  cand.beta <- rnorm(1,0,1) #Check this bit
  parm[n,] <- c(cand,cand.beta,NA)

  ### partitioning covariates into baseline hazard and coefficients
  DC_cand <- DC_clean
  DC_cand$model_extract$cov_co <- cand[-c(1:length(haz_co))]
  DC_cand$model_extract$haz_co <- cand[1:length(haz_co)]

  ### cadidate log Hazard Ratios
  beta.old <- parm[n-1,(length(cand)+1)];beta.old
  beta.new <- parm[n,(length(cand)+1)];beta.new

  ### Prior contribution
  pr.cand <- -dmvnorm(cand,est,sig,log=T)
  pr.old <- log(dnorm(beta.old,0,1000))
  pr.new <- log(dnorm(beta.new,0,1000))

  ### Likelihood evaluation
  l.old <- lik.flexsurvreg(beta.old,DC_cand) + pr.old + pr.cand
  l.new <- lik.flexsurvreg(beta.new,DC_cand)  + pr.new + pr.cand

  ### Accept/Reject
  parm[n,ncol(parm)] <- l.new
  if(!acc(l.old,l.new)) {
    parm[n,(length(cand)+1)] <- beta.old
    parm[n,ncol(parm)] <- l.old
  }

}
parm
}



