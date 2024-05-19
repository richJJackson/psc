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

