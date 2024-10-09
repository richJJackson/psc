#' lp_psc
#'
#' @param x an object of class 'psc'
#' @return linear predictor
#' @details A function to estimate the linear predictor of a 'psc' object
lp_psc <- function(x){
  me <- x$DC_clean$model_extract;me
  dc <- x$DC_clean;dc
  lp <- dc$cov[,1:length(me$cov_co)]%*%me$cov_co
  c(lp)
}

