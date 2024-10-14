#' Personalised Synthetic Controls model fit
#'
#' @param CFM A model of type 'glm' or 'flexsurvspline'
#' @param DC A dataset including columns to match to covariates in the model
#' @param nsim The number of simulations for the MCMC routine
#' @param id Numeric vector stating which patient(s) from the dataset should be included in the analysis.
#'  Defaults to all patients
#' @param trt An optional vector denotin treatment allocations for multiple treatmence comparisons.  Defaults to NULL.
#' @details the \code{pscfit} function compares a dataset ('DC') against a parametric model.
#'   This is done by selecting a likelihood which is identified by the type of CFM that is supplied.
#'   At present, two types of model are supported, a flexible parmaeteric survival model of type 'flexsurvreg'
#'   and a geleneralised linear model of type 'glm'.
#'
#'   Where the CFM is of type 'flexsurvreg' the likeihood supplied is of the form:
#'
#'   \deqn{L(D|\Lambda, \Gamma_i) = \prod^{n}_{i=1} f(t_i | \Lambda, \Gamma_i)^{c_i}
#'   S(t_i|\Lambda, \Gamma_i)^{(1-c_i)}}
#'
#'   Where \eqn{\Lambda} defines the cumulative baseline hazard function,
#'   \eqn{\Gamma} is the linear predictor and \eqn{t} and \eqn{c} are the
#'   event time and indicator variables.
#'
#'   Where the CFM is of the type 'glm' the likelihood supplied is of the form:
#'
#'    \deqn{L(x|\Gamma_i) = \prod^{n}_{i=1} b(x|\Gamma_i) \exp{\{\Gamma_i^T t(x)
#'    - c(\Gamma_i)\} } }
#'
#'   Where \eqn{b(.)}, \eqn{t(.)} and \eqn{c(.)} represent the functions of the
#'   exponential family. In both cases, \eqn{\Gamma} is defiend as:
#'
#'   \deqn{ \Gamma = \gamma x + b}
#'
#'   Where \eqn{\gamma} are the model coefficients supplied by the CFM and b
#'   is the parameter set to measure the difference between the CFM and the DC.
#'
#'   Estimation is performed using a Bayesian MCMC procedure.  Prior distributions
#'    for \eqn{\Gamma} (& \eqn{\Lambda}) are derived directly from the model
#'    coefficients (mean and variance covariance matrix) or the CFM. A bespoke MCMC
#'    routine is performed to estimate \eqn{b}.  Please see '?mcmc' for more detials.
#'
#'    For the standard example where the DC contains information from only a single
#'    treatment, trt need not be specified.  Where comparisons between the CFM and
#'    multiple treatments are require, a covariate of treamtne allocations must be
#'    specified sperately (using the 'trt' option).
#'
#' @return a object of class 'psc' with attributes model.tupe, the cleaned Dataset and the
#'   posterior distribution of the fitted model
#'
#'   Attributes include \itemize{
#'
#'  \item {A 'cleaned' dataset including extracted components of the CFM and the
#'  cleaned DC included in the procedure}
#'  \item {An object defining the class of model (and therefore the procedure
#'  applied - see above)}
#'  \item {A matrix containing the draws of the posterior distributions}
#'  }
#' @import enrichwith mvtnorm survival
#' @example pscfit.example.R
#' @export
pscfit <- function (CFM, DC, nsim = 5000, id = NULL, trt = NULL) {

  ### Cleaning Data
  DC_clean <- dataComb(CFM, DC, id=id, trt = trt)

  ### Starting Parameters
  init <- initParm(CFM = CFM, DC_clean = DC_clean, trt = trt)

  ### MCMC estimation
  mcmc <- pscEst(CFM = CFM, DC_clean = DC_clean, nsim = nsim,
                 start = init$par, trt = trt)

  ### Formatting results
  covnm <- "beta"
  if (!is.null(trt)) {
    df <- data.frame(DC_clean$cov)
    ft <- factor(df$trt)
    covnm <- paste("beta", levels(ft), sep = "_")
  }

  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), covnm,
                   "DIC")
  psc.ob <- list(model.type = class(CFM), DC_clean = DC_clean,
                 posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)
}

