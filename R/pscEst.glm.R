#' Fucntion for estimating initial parameter values  'glm'
#'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param nsim the number of MCMC simulations to run
#' @param start the stating value for
#' @details An MCMC routine for fitting a psc model
#' @return a matrix continig the draws form the posterior ditribution
#' @export
#'
pscEst.glm <- function(CFM,DC_clean,nsim,start,trt=trt){

  cov_co <- DC_clean$model_extract$cov_co;cov_co
  sig <- DC_clean$model_extract$sig;sig
  est <- c(cov_co)
  trt.con <- is.null(trt)

  ####### Bayesian Estimation
  beta <- start
  parm <- matrix(NA, nsim, length(est) + 2)
  parm[1, ] <- c(est, beta, NA)

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
  DC_cand$model_extract$cov_co <- as.numeric(cand)

  ### cadidate log Hazard Ratios
  beta.old <- parm[n-1,(length(cand)+1)];beta.old
  beta.new <- parm[n,(length(cand)+1)];beta.new

  ### Prior contribution
  pr.cand <- -dmvnorm(cand,est,sig,log=T)
  pr.old <- log(dnorm(beta.old,0,1000))
  pr.new <- log(dnorm(beta.new,0,1000))

  if(trt.con){
    ### Likelihood evaluation
    l.old <- lik.glm(beta.old,DC_cand) + pr.old + pr.cand
    l.new <- lik.glm(beta.new,DC_cand)  + pr.new + pr.cand
  }

  if(!trt.con){
    l.old <- lik.flexsurvreg.mtc(beta.old,DC_cand) + pr.old + pr.cand;l.old
    l.new <- lik.flexsurvreg.mtc(beta.new,DC_cand)  + pr.new + pr.cand
  }


  ### Accept/Reject
  parm[n,ncol(parm)] <- l.new
  if(!acc(l.old,l.new)) {
    parm[n,(length(cand)+1)] <- beta.old
    parm[n,ncol(parm)] <- l.old
  }

}
parm
}



