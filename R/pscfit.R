#' Personalised Synthetic Controls
#'
#' @param model A model of type 'glm' or 'flexsurvspline'
#' @param data A dataset including columns to match to covariates in the model
#' @param nsim The number of simulations for the MCMC routine
#' @return a object of class 'psc'
#' @examples
#' psc.ob <- psc(model,data)
#' summary(psc.ob)


pscfit <- function(CFM,DC,nsim=5000){


  ### Getting model type (to inform likelihood)
  model.type <- attributes(CFM)$class;model.type

  if("glm"%in%model.type){

    ## Extracting model components
    model.extract <- model.extract.glm(CFM)

    ## Combining Data
    DC.clean <- data.comb.glm(model.extract,DC)

    ###### Estimation
    ### Frequentist estimation (Starting values)
    freq.est <- optim(beta,lik.ef,cov_co=model.extract$cov_co,DC.clean=DC.clean,family=model.extract$family,method="Brent",lower=-10,upper=10,hessian=T)

    mu.start <- freq.est$par;mu.start

    mcmc <- mcmc.ef(nsim, mu.start,model.extract,DC.clean)


  }


  ### Extracting model components
  if("flexsurvreg"%in%model.type){

    ## Extracting model components
    model.extract <- model.extract.fpm(CFM)

    ## Combining Data
    DC.clean <- data.comb.fpm(model.extract,DC)

    ###### Estimation
    ### Frequentist estimation
    freq.est <- optim(beta,lik.fpm,cov_co=model.extract$cov_co,haz_co=model.extract$haz_co,model.extract=model.extract,DC.clean=DC.clean,method="Brent",lower=-10,upper=10,hessian=T)

    start <- freq.est$par
    se.start <- 1/sqrt(freq.est$hessian)

    ### Bayesian Estimation

    ## MCMC
    mcmc <- mcmc.fpm(nsim,start,model.extract,DC.clean)

  }

  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(model.extract$sig),"beta","DIC")

  ### Add in output here

  ###

  psc.ob <- list("model Type"=model.type,"CFM"=model.extract,"DC"=DC.clean,"posterior"=mcmc)
  psc <- setClass("psc",slots=c("model.type"="character","CFM"="list","DC"="data.frame","posterior"="matrix"))
  class(psc.ob) <- "psc"
  psc.ob

}
