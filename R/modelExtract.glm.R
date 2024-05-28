#' A generic function for extracting model information
#' @param CFM a model of class 'glm'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components
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

