### Utility functions for psc

coef.psc <- function(x){
  y <- x$posterior$beta;y
  qu <- quantile(y,p=c(0.025,0.5,00.975))
  p <- sum(y<0)/length(y)

  res <- as.numeric(c(qu[2],qu[1],qu[3],p))
  names(res) <- c("median","lower","upper","Pr(x<0)")
  res
  }



### Survival Function
surv_fpm <- function(x,beta=0){

  me <- x$CFM
  dc <- x$DC
  time <- dc$outcome$time
  logt <- log(time+1e-06)

  lam <- me$lam
  kn <- me$kn
  cov_co <- me$cov.co
  haz_co <- me$haz_co
  k <- me$k

  linPred <- lp_psc(x)
  adjLP <- mean(linPred) + beta

  z <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
  }

  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  H<- H0*exp(adjLP)
  S <- exp(-H)

  ord <- order(time)
  ret <- list("time"=time[ord],"S"=S[ord])
  ret

}



#### Linear Predictor
lp_psc <- function(x){
  me <- x$CFM
  dc <- x$DC
  lp <- dc$cov%*%me$cov_co
  c(lp)
}


