#' Starting conditions for Bayesian MCMC estimation procedures in 'pscfit'
#' A procedure which sets the starting conditions for MCMC estimation
#'
#' @param pscOb an pscOb object which has been passed through pscData() and
#' init() functions
#' @param nsim the number of MCMC simulations to run
#' @param nchain Number of chains to use for analysis
#' @return An updated set of attributes for the pscOb which includes
#' @details
#' A procedure which sets the starting conditions for MCMC estimation including
#' defining starting estimates, setting a matrix for draws to be save in and
#' defining, target and prior distributions and deifnign the posterior
#' desitribution from the CFM.  This also sets the number of cores to be used
#' for estimation where parallel computing is applied.
#' @export

pscEst_start <- function(pscOb,nsim,nchain){

  #starting parameters
  if(!is.null(pscOb$trt)){
    pscOb$start.mu <- rmvnorm(1,pscOb$start.mu,sigma=c(pscOb$start.sd^2))
  }

  if(is.null(pscOb$trt)){
    pscOb$start.mu <-rnorm(1,pscOb$start.mu,pscOb$start.sd)
  }


  ### Setting up matrix to save draws
  draws <- matrix(NA,nsim,length(pscOb$co)+length(pscOb$start.mu)+1)
  draws[1,]<- c(pscOb$co,pscOb$start.mu,NA)
  beta.nm <- paste("beta",1:length(pscOb$start.mu),sep="_")
  colnames(draws) <- c(names(pscOb$co),beta.nm,"likEst")

  ## adding hazard parameters onto covariates for flexsurvsreg models
  cfmPost <- function(x) c(rmvnorm(x,pscOb$co,pscOb$sig))

  #starting parameters
  if(!is.null(pscOb$trt)){
    target <- function(x) c(rmvnorm(x,pscOb$start.mu,pscOb$start.sd*2))
  }

  if(is.null(pscOb$trt)){
    target <- function(x) c(rnorm(x,pscOb$start.mu,pscOb$start.sd*2))
  }

  # prior distributions
  betaPrior <- function(x) dmvnorm(x,rep(0,length(x)),diag(length(x))*1000,log=T)

  # number of cores (for parallel computing of multiple chians)
  dcores <- detectCores()
  if(dcores>3) {
    ncores <- min(round(dcores*.75),nchain)
  }

  ## Returning estimation object
  pscOb$cfmPost <- cfmPost;pscOb
  pscOb$target <- target;pscOb
  pscOb$betaPrior <- betaPrior

  pscOb$ncores <- ncores
  pscOb$draws <- draws;pscOb

  ## Returning object
  pscOb
}
