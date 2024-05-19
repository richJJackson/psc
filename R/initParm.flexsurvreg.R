initParm.flexsurvreg <- function(CFM,DC_clean){
  ip <- optim(beta, lik.flexsurvreg, DC_clean=DC_clean, method = "Brent", lower = -10,
        upper = 10, hessian = T)
  return(ip)
  }
