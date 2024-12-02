### Estimation Issues


library(psc)
library(survival)

setwd("~/Documents/GitHub/psc/Develop")

### UK COMPASS

data <- read.csv("ukcData.csv")
load("UKCmodel.Rdata")
flexspline_model5

pscfit(flexspline_model5,data)


## Added a 'silent=T' option to the try wrapper so that 'solved' the problem but
## plenty of 'failures here' - need a closer look


######## TACTICS

## step 1 - compare data to model

data <- read.csv("TACTICS data final.csv")
model <- load("fpm.tace.R")




### Model Object
### Setting
P <- "Patients undergoing surgery for a complex aortic aneurysm"
I <- "Open Surgery"
C <- "Not Included in Model"
O <- "Overall Survival"
sett <- cfmSett(P,I,C,O)

cfm.ob <- pscCFM(fpm.tace,setting=sett)
cfm.ob

##### PSC


#################
## Data cleaning

data$OS_months <- as.numeric(as.Date(data$Date,"%d/%m/%Y")-as.Date(data$Date.of.randomization,"%d/%m/%Y"))/30.44
data$status <- data$Death.1.Alive.0.2020.7.31Cutoff

### defining survival object
data$s.ob <- Surv(data$OS_months,data$status)



### Cleaning Data

### Removing patient with HBV and HCV
data <- data[-which(data$"HBｓ.Ag"=="+"&data$HCV.Ab=="+"),]

data$tumour.number <- cut(data$Number.of.tumors,c(0,1.5,20),c(0,1))
data$tum.siz <- log(data$Maximum.Tumor.size.cm.+0.1,10)
data$afp <- log(as.numeric(data$AFP..ng.ml.)+1,10)
data$alb <- data$ALB..g.dl.*10
data$bil <- log(data$T.Bil..mg.dl.*10,10)
data$vi <- "No"
data$cp <- "Child-Pugh A"

data$hcv <- 0
data$hcv[which(data$HCV.Ab=="+")] <- 1

data$hbv <- 0
data$hbv[which(data$HBｓ.Ag=="+")] <- 1

#data$aetOther[which(data$"HCV.Ab"=="-"&data$"HBｓ.Ag"=="-")] <- 1
data$ecog <- data$ECOG.PS

data$tumour.number <- factor(data$tumour.number,labels=c("Single","Multiple"))


data$time <- data$OS_months
data$cen <- data$status


pscfit(fpm.tace,data)





pscCFM



dataSumm

####


dataSumm <- function(x){

  cl.x <- class(x)

  if(cl.x%in%c("character","factor")){
    x <- factor(x)
    lev <- levels(x)
    tb <- table(x)
    ret <- c("Class"=cl.x,tb)
  }

  if(cl.x%in%c("integer","numeric")){
    x <- as.numeric(x)
    quant <- round(quantile(x,c(0.5,0.25,0.75)),2)
    minx <- min(x,na.rm=T)
    maxx <- max(x,na.rm=T)
    miqr <- paste(quant[1]," (",quant[2],", ",quant[3],")",sep="")
    ret <- c("Class"=cl.x,"min"=minx,"max"=maxx,"miqr"=miqr)
    ret
  }

  ret

}



######






CFM <- flexspline_model5
DC <- data
id <- NULL
trt <- NULL
nsim <- 500

initParm.flexsurvreg(CFM,DC_clean,trt)


pscfit <- function (CFM, DC, nsim = 5000, id = NULL, trt = NULL) {
  DC_clean <- dataComb(CFM, DC, id = id, trt = trt)
  init <- initParm(CFM = CFM, DC_clean = DC_clean, trt = trt)
  mcmc <- pscEst(CFM = CFM, DC_clean = DC_clean, nsim = nsim,
                 start = init$par, trt = trt)


  covnm <- "beta"
  if (!is.null(trt)) {
    df <- data.frame(DC_clean$cov)
    ft <- factor(df$trt)
    covnm <- paste("beta", levels(ft), sep = "_")
  }
  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), covnm,
                   "DIC")
  psc.ob <- list(model.type = class(CFM), DC_clean = DC_clean,
                 posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)
}



initParm.flexsurvreg <- function(CFM,DC_clean,trt=NULL){

  if(is.null(trt)){
    beta<- 0
    ip <- optim(beta, lik.flexsurvreg, DC_clean=DC_clean, method = "Brent", lower = -10,
                upper = 10, hessian = T)
  }

  if(!is.null(trt)){
    beta <- rep(0,length(levels(factor(trt))))
    ip <- optim(beta, lik.flexsurvreg.mtc, DC_clean=DC_clean, method = "BFGS", hessian = T)
  }

  class(ip) <- class(CFM)
  return(ip)
}


start <- init$par

pscEst.flexsurvreg <- function(CFM,DC_clean,nsim,start,trt=trt){

  cov_co <- DC_clean$model_extract$cov_co;cov_co
  haz_co <- DC_clean$model_extract$haz_co;haz_co
  sig <- DC_clean$model_extract$sig
  lam <- DC_clean$model_extract$lam
  kn <- DC_clean$model_extract$kn
  time <- DC_clean$out$time;time
  cen <- DC_clean$out$cen
  cov <- DC_clean$cov
  est <- c(haz_co,cov_co);est

  trt.con <- is.null(trt);trt.con

  ####### Bayesian Estimation
  beta <- start
  parm <- matrix(NA,nsim,length(est)+length(beta)+1)
  parm[1,]<- c(est,beta,NA);parm[1,]

  ## Progress Bar
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)

  for(n in 2:nsim){

    ## progress bar
    setTxtProgressBar(pb, n)

    ### Drawing Samples
    cand <- rmvnorm(1,est,sig)
    cand.beta <- rnorm(length(beta),0,1) #Check this bit
    parm[n,] <- c(cand,cand.beta,NA)

    ### partitioning covariates into baseline hazard and coefficients
    DC_cand <- DC_clean
    DC_cand$model_extract$cov_co <- cand[-c(1:length(haz_co))]
    DC_cand$model_extract$haz_co <- cand[1:length(haz_co)]


    ### cadidate log Hazard Ratios
    beta.old <- parm[n-1,-c(1:length(cand),ncol(parm))];beta.old
    beta.new <- parm[n,-c(1:length(cand),ncol(parm))];beta.new

    ### Prior contribution
    pr.cand <- -dmvnorm(cand,est,sig,log=T)

    pr.old <- dmvnorm(beta.old,rep(0,length(beta)),diag(length(beta))*1000,log=T)
    pr.new <- dmvnorm(beta.new,rep(0,length(beta)),diag(length(beta))*1000,log=T)

    ### Likelihood evaluation
    if(trt.con){
      l.old <- lik.flexsurvreg(beta.old,DC_cand) + pr.old + pr.cand;l.old
      l.new <- lik.flexsurvreg(beta.new,DC_cand)  + pr.new + pr.cand;l.new
    }

    if(!trt.con){
      l.old <- lik.flexsurvreg.mtc(beta.old,DC_cand) + pr.old + pr.cand;l.old
      l.new <- lik.flexsurvreg.mtc(beta.new,DC_cand)  + pr.new + pr.cand
    }

    ### Accept/Reject
    parm[n,ncol(parm)] <- l.new
    if(!acc.test(l.old,l.new)) {
      parm[n,-c(1:length(cand),ncol(parm))] <- beta.old
      parm[n,ncol(parm)] <- l.old
    }

    #print(l.old)
   #print(l.new)

  }

  parm
}


beta.old
beta.new

DC_cand


l.old
l.new


l.old <- lik.flexsurvreg(beta.old,DC_cand) + pr.old + pr.cand;l.old
l.new <- lik.flexsurvreg(beta.new,DC_cand)  + pr.new + pr.cand;l.new


acc.test <- function(old,new){
  ret <- FALSE
  r <- runif(1)
  e <-exp(old-new)
  try(if(e>r) ret <- TRUE,silent=T)
  ret
}


DC_clean <- DC_cand

lik.flexsurvreg <- function(beta,DC_clean){
  lam <- DC_clean$model_extract$lam;lam
  kn <- DC_clean$model_extract$kn;kn
  k <- DC_clean$model_extract$k;k
  haz_co <- DC_clean$model_extract$haz_co;haz_co
  cov_co <- DC_clean$model_extract$cov_co;cov_co

  time <- DC_clean$out$time;time
  cen <- DC_clean$out$cen
  cov <- DC_clean$cov

  logt <- log(time)
  lp <- cov%*%cov_co;lp
  z <- NULL
  z_h <- NULL

  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)

    zt_h <- (modp(logt-kn[(i+1)])^2 - lam[(i+1)]*modp(logt-kn[1])^2 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^2)
    z_h <- cbind(z_h,zt_h)

  }


  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)]);H0
  h0 <- (H0/time)*(haz_co[2]+3*z_h%*%haz_co[3:(2+k)]);h0

  H<- H0*exp(lp+beta)
  h<- h0*exp(lp+beta)
  S <- exp(-H)
  f <- S*h
  cbind(H0,h0,S,f,log(f),lp,time,cen)[100:120,]
  f
  l <- sum(cen*log(f+1e-16) + (1-cen)*log(S+1e-16))

  options(warn=-1)
  ll <- rowSums(cbind(cen*log(f+1e-16),(1-cen)*log(S+1e-16)),na.rm=T)
  sum(ll)
  na.rm=T)

}

