### cfmEst

### Function to estimate outcomes from a CFM model - useful for Rshiny development

### set working directoy for psc
setwd("/Users/richardjackson/Documents/GitHub/psc")
devtools::load_all()


### survival model
CFM <- psc::gemCFM

### estimate survival function from model (assumes all covariates = 0)
s.est <- cfmEst(CFM)

### estimate estimate with known covariate values

smod$cov_co;#this shows us what covariates we need
smod$cov_class # this tells us how they should be specified
smod$cov_lev # this tells us how they should be specified

## new data
t <- factor(c(2),levels=c(2,3,4))
grade <- factor(c(2),levels=c(1,2,3))
nodes <- factor(c(1),levels=c(1,2))
lca199 <- c(1)

newData <- model.matrix(~t+grade+nodes+lca199)[,-1]; ## note this 
## gets the data in the right format for us - the '[,-1]' is to remove the intercept
s.est2 <- cfmEst(CFM,cov=newData)


### So you can see how the two survival estimates change....
plot(s.est$time,s.est$S,typ="l",lwd=4,col=4,ylim=c(0,1))
lines(s.est2$time,s.est2$S,typ="l",lwd=4,col=5)






###################################################
###################################################
############# Functions below!!!!!
###################################################
###################################################


cfmEst <- function(CFM,maxTime=60,beta=0,cov=NULL){
  
  lam <- smod$lam
  kn <- smod$kn
  k <- smod$k
  haz_co <- smod$haz_co
  cov_co <- smod$cov_co

  s.est <-  spline_surv_est(lam=lam,kn=kn,k=k,haz_co=haz_co,
                  cov_co=cov_co,cov=cov,maxTime=maxTime,beta=beta)

  s.est
  
}



spline_surv_est <- function(lam,kn,k,haz_co,cov_co,cov=NULL,maxTime=60,beta=0){
  
  if(is.null(cov)) cov <- rep(0,length(cov_co))
  
  tm <-   seq(0.01,maxTime,length=500)
  logt <- log(tm)
  
  
  cov
  cov_co
  lp <- c(cov%*%cov_co);lp
  
  z <- NULL
  z_h <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
    
    zt_h <- (modp(logt-kn[(i+1)])^2 - lam[(i+1)]*modp(logt-kn[1])^2 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^2)
    z_h <- cbind(z_h,zt_h)
    
  }
  
  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  h0 <- (H0/tm)*(haz_co[2]+3*z_h%*%haz_co[3:(2+k)])
  
  H<- H0*exp(lp+beta)
  h<- h0*exp(lp+beta)
  S <- exp(-H)
  f <- S*h
  
  ret <- data.frame("time"=tm,"S"=S,"f"=f)
  ret
}





