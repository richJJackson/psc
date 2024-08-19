### Example of PSC in R


#.rs.restartR()
rm(list=ls())
remove.packages("psc")
devtools::install_github("RichJJackson/psc",ref="Version-1.0")
library(psc)



## loading data and model
setwd("Example")

### Getting data
load("exData.R")
load("bin.data.R")


### Getting model
load("model.R")
load("bin.mod.R")



### Getting data and saving
setwd("/Volumes/RICHJ23/Fellowship/Methodology/Data and Models/Data")

dir()
data[1:3,]


data$trt <- round(runif(nrow(data),0.5,3.5))-1
load("model.rda")


use_data(surv.mod)
surv.mod <- model

load("test.bmodelload("test.bin.R")
load("test.con.R")
load("test.count.R")

setwd("~/Documents/GitHub/psc/Data")
use_data(data,overwrite=TRUE)
use_data(con.mod)
use_data(count.mod)
ls()

library(devtools)
pscfit(model,data)

#### Survivaldata#### Survival Model

### Running basic
res <- pscfit(ukoss.mod,edata)

plot(res)
coef(res)
print(res)
summary(res)



### Running basic
res <- pscfit(model,data)

data$event <- data$cen
data$os <- data$time


data$count <- round(runif(nrow(data),0,5))

res_bin <- pscfit(bin.mod,data)
res_bin <- pscfit(con.mod,data)
res_bin <- pscfit(count.mod,data)

use_data(data,overwrite=T)

### Checking each outcome



CFM <- bin.mod

DC <- data
trt <- NULL
id <- NULL

pscfit <- function (CFM, DC, nsim = 5000, id = NULL, trt = NULL) {

  ### Cleaning Data
  DC_clean <- dataComb(CFM, DC, id=id, trt = trt)

  ### Starting Parameters
  init <- initParm(CFM = CFM, DC_clean = DC_clean, trt = trt)

  ### MCMC estimation
  mcmc <- pscEst(CFM = CFM, DC_clean = DC_clean, nsim = nsim,
                 start = init$par, trt = trt)

  ### Formatting results
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






plot(res)
coef(res)
print(res)
summary(res)




## Adding
s.ob <- Surv(data$time,data$cen)
sfit <- survfit(s.ob~1)
lines(sfit,col="forestgreen",lwd=3)





# Individaul treatment effects (takes a while)
ite <- psc_ite(model,data)
ite <- ite[order(ite[,1]),]

plot(density(ite[,1]))

plot(ite[,1],dum,xlim=c(-3,3))
segments(ite[,2],dum,ite[,3],dum)

psc_all <- pscfit(model,data)

coef(psc_all)
abline(v=c(0.1,0.57,0.36))
median(ite[,1])






### Running with missing covariate
data_err_1 <- data[,-3]
res <- psc(fpm,data_err_1)

### Running with missing data (should give warning)
data_err_2 <- data
data_err_2[2:5,2:5] <- NA
res <- psc(fpm,data_err_2)

### Running with misnamed outcome
data_err_3 <- data
names(data_err_3)[which(names(data_err_3)=="time")] <- "surv_time"
res <- psc(fpm,data_err_3)





### Error's

# Warning on Simulation size
# Warning on data distribution
  # Continuous outside of range
  # Categorical not included











