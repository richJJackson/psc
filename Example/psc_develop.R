### Example of PSC in R


#.rs.restartR()
rm(list=ls())
remove.packages("psc")
devtools::install_github("RichJJackson/psc",ref="Version-1.0")
library(psc)



model.extract()## loading data and model
setwd("Example")


setwd("~/Documents/GitHub/psc/R/")

### Loading Functions
di <- dir()
for(i in di){source(i)}


###########################


### Survival Example
res_surv_1 <- pscfit(surv.mod,data,nsim=100)

### MTC
res_surv_2 <- pscfit(bin.mod,data,trt=data$trt,nsim=100)

### ID
res_surv_3 <- pscfit(bin.mod,data,id=c(1:15),nsim=100)

### MTC & ID
res_surv_4 <- pscfit(bin.mod,data,id=c(1:15),trt=trt,nsim=100)

###########################


### Binary Example
res_bin_1 <- pscfit(bin.mod,data,nsim=100)

### MTC
res_bin_2 <- pscfit(bin.mod,data,trt=data$trt,nsim=100)

### ID
res_bin_3 <- pscfit(bin.mod,data,id=c(1:15),nsim=100)

### MTC & ID
res_bin_4 <- pscfit(bin.mod,data,id=c(1:15),trt=trt,nsim=100)



###########################


### Continuous Example
res_cont_1 <- pscfit(con.mod,data,nsim=100)

### MTC
res_cont_2 <- pscfit(con.mod,data,trt=data$trt,nsim=100)

### ID
res_cont_3 <- pscfit(con.mod,data,id=c(1:15),nsim=100)

### MTC & ID
res_cont_4 <- pscfit(con.mod,data,id=c(1:15),trt=trt,nsim=100)




### Count Example
res_count_1 <- pscfit(count.mod,data,nsim=100)

### MTC
res_count_2 <- pscfit(count.mod,data,trt=data$trt,nsim=100)

### ID
res_count_3 <- pscfit(count.mod,data,id=c(1:15),nsim=100)

### MTC & ID
res_count_4 <- pscfit(count.mod,data,id=c(1:15),trt=trt,nsim=100)




### post-estimation commands


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





