#' A generic function for extracting model information
#'
#' This function extracts model information for use with the pscfit.R function.
#'
#' @param CFM a model of class 'flexsurvreg'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components.  Included are
#' \itemize{
#' \item{model.frame a dataset of the covareits used to create the model}
#' \item{cov_co: covariate coefficients}
#' \item{sig: variance-covariance matrix}
#' \item{haz_co: hazard parameter coefficients}
#' \item{k: number of knots}
#' \item{knots: knot position}
#' \item{lam: lambda parameter}
#' \item{form: model formula}
#' }
#' @export
modelExtract.flexsurvreg <- function(CFM){
  co <- CFM$coefficients
  k <- CFM$k
  kn <- CFM$knots
  sig <- vcov(CFM)
  max(kn)

  lam <- (max(kn)-kn)/(max(kn)-min(kn))

  form <- formula(CFM)
  mf <- model.frame(CFM)

  n_haz_co <- k+2
  haz_co <- co[1:n_haz_co]
  cov_co <- co[(n_haz_co+1):length(co)]

  ret <- list("model.frame"=mf,"cov_co"=cov_co,"sig"=sig,"haz_co"=haz_co,"k"=k,"kn"=kn,"lam"=lam,"formula"=form)
  return(ret)
}

