#' A generic function for extracting model information
#' @param CFM a model of class 'glm'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components.  Included are
#' \itemize{
#' \item{model.frame a dataset of the covareits used to create the model}
#' \item{cov_co: covariate coefficients}
#' \item{sig: variance-covariance matrix}
#' \item{form: model formula}
#' \item{family: model family}
#' \item{out.nm: outcome covariates names}
#' }
#' @export
#'
modelExtract.glm <- function(CFM){
  co <- CFM$coefficients;co
  fam <- CFM$family
  sig <- vcov(CFM)

  form <- formula(CFM)
  mf <- model.frame(CFM)

  list("model.frame"=mf,"cov_co"=co,"sig"=sig,"formula"=form,"family"=fam,"out.nm"=names(mf)[1])

}

