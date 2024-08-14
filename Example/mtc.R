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




### Checking it works
trt <- round(runif(nrow(data)))
psc1 <- pscfit(CFM,DC,trt=trt,nsim=50)
psc2 <- pscfit(CFM,DC,nsim=50)
x <- psc1




coef.psc <- function(x, ...){

  cl.nm <- attributes(x$DC_clean$model_extract$sig)$dimnames[[1]]
  y <- x$posterior[,which(!names(x$posterior)%in%cl.nm)]

  lap <- lapply(y,quantile,na.rm=T,p=c(0.025,0.5,00.975))
  lap <- t(matrix(unlist(lap),3,ncol(y)))
  p <- colSums(y<0)/nrow(y)

  res <- data.frame(cbind(lap,p))
  names(res) <- c("median","lower","upper","Pr(x<0)")
  as.matrix(res)
}

coef(psc1)
plot(psc2)

print(psc1)


### Old one should still work!!
psc_old <- pscfit(CFM,DC)




psc1$posterior[1:3,]
psc2$posterior[1:3,]













### Change this - if mtc then need differente outcomes


















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





