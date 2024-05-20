#.rs.restartR()
rm(list=ls())
remove.packages("psc")
devtools::install_github("RichJJackson/psc",ref="2024_05_15")
library(psc)


## loading data and model
setwd("Example")

### Getting data
load("exData.R")

### Getting model
load("model.R")




initParm <- function(CFM,DC_clean){
  ip <- optim(beta, lik.flexsurvreg_np, DC_clean=DC_clean,odds=T, method = "Brent", lower = -10,
              upper = 10, hessian = T)

  ip <- optim(beta, lik.flexsurvreg, DC_clean=DC_clean, method = "Brent", lower = -10,
              upper = 10, hessian = T)

  class(ip) <- class(model_extract)
  return(ip)
}
ip



pscfit_np <- function (CFM, DC, nsim = 5000, id = NULL){

  # Creating 'cleaned' dataset for comparison
  DC_clean <- dataComb(CFM=CFM,DC=DC,id=NULL)



  # Initial Estimates using Optims
  init <- initParm(CFM=CFM,DC_clean=DC_clean)

  # estimation
  mcmc <- pscEst(CFM=CFM,DC_clean=DC_clean,nsim=nsim,start=init$par)

  ## Cleaning output
  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), "beta", "DIC")
  psc.ob <- list("model.type"=class(CFM),DC_clean=DC_clean, posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)
}

