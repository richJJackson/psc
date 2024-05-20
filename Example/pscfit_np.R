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
