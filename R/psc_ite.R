## Wrapper function for individual treatment effects

psc_ite <- function(model,data){

  nr <- nrow(data)
  res <- NULL

  for(i in 1:nr){
    fit_i <- pscfit(model,data,id=i)
    co_i <- coef(fit_i)
    res <- rbind(res,co_i)
    }
  res
}




