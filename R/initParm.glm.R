#' Fucntion for estimating initial parameter values  'flexsurvreg'
#'
#' @param DC_clean a cleaned dataset obsect obtained using dataComb.flexsurvreg
#' @details
#' This function takes the liklihood for a 'flexsurvreg' model and uses 'optim'
#'   to fit the likelihood.
#' @return an 'optim' output giving the parameter values to be supplied as a
#'   starting value for the mcmc routine.
#' @export
#'
initParm.glm <- function(CFM,DC_clean){
  ip <- optim(beta, lik.glm, DC_clean=DC_clean, method = "Brent", lower = -10,
        upper = 10, hessian = T)
  return(ip)
  }

