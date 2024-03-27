

### laoding packages
library(mvtnorm)
library(survival)
library(flexsurv)
library(enrichwith)
library(devtools)
devtools::install_github("richjjackson/psc")
library(psc)



### Loading Data and model
load("data.R")
load("model.R")

psc <- psc(CFM,DC,nsim=25000)
summary(psc[,17])


