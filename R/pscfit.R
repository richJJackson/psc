#' Personalised Synthetic Controls
#'
#' @param model A model of type 'glm' or 'flexsurvspline'
#' @param data A dataset including columns to match to covariates in the model
#' @param nsim The number of simulations for the MCMC routine
#' @param id Numeric vector stating which patient(s) from the dataset should be included in the analysis.
#'  Defaults to all patients
#' @param trt An optional vector denotin treatment allocations for multiple treatmence comparisons.  Defaults to NULL.
#' @details the \code{pscfit} function compares a dataset ('DC') against a parametric model.
#'   This is done by forming a likelihood based on the type of model which is supplied.
#'   Estimation is performed using a Bayesian MCMC procedure.  Prior distributions are
#'   derived from the vector of coefficients and variance covariance of the supplied model
#' @return a object of class 'psc' with attributes model.tupe, the cleaned Dataset and the
#'   posterior distribution of the fitted model
#' @examples
#' psc.ob <- psc(model,data)
#' summary(psc.ob)
#' @export
pscfit <- function (CFM, DC, nsim = 5000, id = NULL),trt=NULL){

  ### Cleaning data
  DC_clean <- dataComb(CFM,DC,trt=trt);DC_clean$cov[1:3,]

  # Initial Estimates using Optims
  init <- initParm(CFM=CFM,DC_clean=DC_clean,trt=trt);init

  # Estimation
  mcmc <- pscEst(CFM=CFM,DC_clean=DC_clean,nsim=nsim,start=init$par,trt=trt)

  covnm <- "beta"

  if(!is.null(trt)){
    df <- data.frame(DC_clean$cov)
    ft <- factor(df$trt)
    covnm <- paste("beta",levels(ft),sep="_")
  }

  ## Cleaning output
  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), covnm,"DIC")
  psc.ob <- list("model.type"=class(CFM),DC_clean=DC_clean, posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)
