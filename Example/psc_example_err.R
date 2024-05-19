### Example of PSC in R


#.rs.restartR()
rm(list=ls())
remove.packages("psc")
devtools::install_github("RichJJackson/psc",ref="2024_05_15")
library(psc)


## loading data and model
setwd("Example")

### Getting data
load("exData.R")

### Getting model
load("model.R")

class(model)

#### Survival Model

### Running basic
res <- pscfit(model,data)


CFM <- model
DC <- data

pscfit <- function (CFM, DC, nsim = 5000, id = NULL){

    # Creating 'cleaned' dataset for comparison
    DC_clean <- dataComb(CFM=CFM,DC=DC,id=NULL)

    # Initial Estimates using Optims
    init <- initParm(CFM=CFM,DC_clean=DC_clean)

    # estimation
    mcmc <- pscEst(CFM=CFM,DC_clean=DC_clean,nsim=nsim,start=init$par)

    ## Cleaning output
    mcmc <- data.frame(mcmc)
    names(mcmc) <- c(colnames(model.extract$sig), "beta", "DIC")
    psc.ob <- list(DC_clean=DC_clean, posterior = mcmc)
    class(psc.ob) <- "psc"
    return(psc.ob)
}





class(res)
attributes(res)

res$model.type

plot(res)
plot.psc(res)

coef(res)
coef.psc(res)


print(res)
print.psc(res)

summary(res)
summary.psc(res)


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











