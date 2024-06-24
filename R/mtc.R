### multiple treatment comparisons
remove.packages("psc")
library(devtools)
install_github("RichJJackson/psc",ref="2024_05_15")
library(psc); pscfit

## loading data
load("/Volumes/RICHJ23/Projects/Cancer/HCC/EURAB/Data/data2.R")
data <- cleaned

## loading data and model
setwd("/Users/richardjackson/Desktop/pscCheck")

### Getting model
load("model.R")


model

# Data cleaning
## VI is not available - need different assumptions
data$vi <- rbinom(nrow(data),1,0.33)

data$ecog <- factor(data$ECOG)
data$ecog[which(data$ecog==2)] <- 1

data$allmets <- as.numeric(data$EHS)-1
data$logafp <- log(data$AFP+1)
data$alb <- data$ALB
data$aet <- ("HBV")
data$aet[which(data$HCV==1)] <- ("HCV")
data$aet[which(data$Other==1)] <- ("Other")
data$age60 <- data$AGE - 60
data$logcreat  <- log(data$CREAT)
data$logast <- log(data$AST)


data$time <- data$dth_time
data$cen <- data$dth_cen


## No creatinine/AST - need different assumptions


### Checking it works
psc <- pscfit(model,data)


plot(psc)
exp(summary(psc))





######################################
######################################
######################################
##### Functions


DC <- data
CFM <- model

### Creating dummy 'treatment' covariate
trt <- round(runif(nrow(data)))
#
trt <-  NULL

pscfit_mtc <- function (CFM, DC, nsim = 5000, id = NULL,trt=NULL){

  ### Cleaning data
  DC_clean <- dataComb(CFM,DC,trt=trt);DC_clean$cov[1:3,]

  # Initial Estimates using Optims
  init <- initParm(CFM=CFM,DC_clean=DC_clean,trt=trt);init

  # Estimation
  mcmc <- pscEst(CFM=CFM,DC_clean=DC_clean,nsim=nsim,start=init$par,trt=trt)

  covnm <- "beta"

  if(!is.null(trt)){
    df <- data.frame(DC_clean$cov)
    ft <- factor(df$trt)
    covnm <- paste("beta",levels(ft),sep="_")
  }

  ## Cleaning output
  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), covnm,"DIC")
  psc.ob <- list("model.type"=class(CFM),DC_clean=DC_clean, posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)

}


pscfit <- function (CFM, DC, nsim = 5000, id = NULL){

  # Creating 'cleaned' dataset for comparison
  DC_clean <- dataComb(CFM=CFM,DC=DC,id=id)

  # Initial Estimates using Optims
  init <- initParm(CFM=CFM,DC_clean=DC_clean,trt=NULL)

  # estimation
  mcmc <- pscEst(CFM=CFM,DC_clean=DC_clean,nsim=nsim,start=init$par,trt=NULL)

  ## Cleaning output
  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), "beta", "DIC")
  psc.ob <- list("model.type"=class(CFM),DC_clean=DC_clean, posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)
}







### Checking it works
trt <- round(runif(nrow(data)))
psc1 <- pscfit_mtc(CFM,DC,trt=trt,nsim=50)
psc2 <- pscfit_mtc(CFM,DC,trt=NULL,nsim=50)


### Old one should still work!!
psc_old <- pscfit(CFM,DC)











dataComb <- function(CFM,DC,id=NULL,trt=NULL){
  UseMethod("dataComb")
}



dataComb.flexsurvreg <- function(CFM,DC,id=NULL,trt=NULL){

  ### removing response and weights
  model_extract <- modelExtract(CFM);model_extract
  mf <- model_extract$model.frame
  term.nm <- names(mf)
  term.nm <- term.nm[-c(1,length(term.nm))];term.nm
  attributes(CFM)


  ### ERROR CHECK: Selecting data from DC
  data_unavail_id  <- which(!term.nm%in%names(DC))
  data_unavail <- term.nm[data_unavail_id]
  if(length(data_unavail_id)!=0) stop(paste("Covariate '",data_unavail,"' is included in the model but not the dataset",sep=""))

  ### Making sure 'time' and 'cen'
  out.id <- which(names(DC)%in%c("time","cen"))
  if(length(out.id)!=2) stop("Please ensure covariates to define the outcome are labeled as 'time' and 'cen'")

  ### Creating model matrix (adding resposne to covariates)
  DC <- data.frame(cbind(0,DC))
  names(DC)[1] <- names(mf)[1]
  DC[,1] <- Surv(DC$time,DC$cen)

  ### Adding treatment variable (if not null)
  if(!is.null(trt)) {
    if("trt"%in%names(DC)){
      DC <- DC[,-which(names(DC)=="trt")]
    }
    DC <- cbind(DC,trt)
    term.nm <- c(term.nm,"trt")
    }

  ### Finding missing data
  DC2 <- DC[,which(names(DC)%in%c(term.nm,"time","cen"))]
  miss.id <- unique(which(is.na(DC2),arr.ind=T)[,1])

  if(length(miss.id)>0) {
    DC <- DC[-miss.id,]
    warning(paste(length(miss.id),"rows removed due to missing data in dataset"))
  }

  ### Estimating linear predictor
  dc_mm <- model.matrix(model_extract$formula,data=DC)[,-1]

  if(!is.null(trt)) dc_mm <- cbind(dc_mm,"trt"=DC$trt)

  out <- data.frame("time"=DC$time,"cen"=DC$cen)

  if(!is.null(id)){
    dc_mm <- dc_mm[id,]
    out <- out[id,]
  }


  ret <- list("model_extract"=model_extract,"cov"=dc_mm,"outcome"=out)
  ret
}




### Change this - if mtc then need differente outcomes
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

  class(ip) <- class(model_extract)
  return(ip)
  }



lik.flexsurvreg.mtc <- function(beta,DC_clean){
  lam <- DC_clean$model_extract$lam
  kn <- DC_clean$model_extract$kn
  k <- DC_clean$model_extract$k
  haz_co <- DC_clean$model_extract$haz_co
  cov_co <- DC_clean$model_extract$cov_co

  time <- DC_clean$out$time;time
  cen <- DC_clean$out$cen
  cov <- DC_clean$cov


  trt.id <- which(colnames(cov)=="trt")
  trt <- cov[,trt.id]
  cov <- cov[,-trt.id]

  trt <- factor(trt)
  lev <- levels(trt)

  if(length(beta)!=length(lev)) stop("beta does not match numebr of levels in treatment")
  Beta <- model.matrix(~-1+trt)%*%beta


  logt <- log(time)
  lp <- cov%*%cov_co

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
  h0 <- (H0/time)*(haz_co[2]+3*z_h%*%haz_co[3:(2+k)])

  H<- H0*exp(lp+Beta)
  h<- h0*exp(lp+Beta)
  S <- exp(-H)
  f <- S*h

  l <- sum(cen*log(f+1e-16) + (1-cen)*log(S+1e-16))
  -l

}



pscEst <- function(x,...){
  UseMethod("pscEst")
}




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

    trt.con <- is.null(trt)

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
        l.old <- lik.flexsurvreg(beta.old,DC_cand) + pr.old + pr.cand
        l.new <- lik.flexsurvreg(beta.new,DC_cand)  + pr.new + pr.cand
      }

      if(!trt.con){
        l.old <- lik.flexsurvreg.mtc(beta.old,DC_cand) + pr.old + pr.cand;l.old
        l.new <- lik.flexsurvreg.mtc(beta.new,DC_cand)  + pr.new + pr.cand
      }

      ### Accept/Reject
      parm[n,ncol(parm)] <- l.new
        if(!acc(l.old,l.new)) {
          parm[n,-c(1:length(cand),ncol(parm))] <- beta.old
          parm[n,ncol(parm)] <- l.old
        }

      }

  parm
}










pscfit <- function (CFM, DC, nsim = 5000, id = NULL,trt=NULL){

  # Creating 'cleaned' dataset for comparison
  DC_clean <- dataComb(CFM=CFM,DC=DC,id=id,trt=trt)

  # Initial Estimates using Optims
  init <- initParm(CFM=CFM,DC_clean=DC_clean,trt=trt)

  # estimation
  mcmc <- pscEst(CFM=CFM,DC_clean=DC_clean,nsim=nsim,start=init$par)

  ## Cleaning output
  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), "beta", "DIC")
  psc.ob <- list("model.type"=class(CFM),DC_clean=DC_clean, posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)
}





