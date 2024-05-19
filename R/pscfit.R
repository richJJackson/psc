#' Personalised Synthetic Controls
#'
#' @param model A model of type 'glm' or 'flexsurvspline'
#' @param data A dataset including columns to match to covariates in the model
#' @param nsim The number of simulations for the MCMC routine
#' @param id Numeric vector stating which patient(s) from the dataset should be included in the analysis.  Defaults to all patients.
#' @return a object of class 'psc'
#' @examples
#' psc.ob <- psc(model,data)
#' summary(psc.ob)
#' @export
pscfit <- function (CFM, DC, nsim = 5000, id = NULL){

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
