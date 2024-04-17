### Example of PSC in R


library(devtools)
#devtools::install_github("RichJJackson/psc")
library(psc)
library(survival)
library(mvtnorm)

## loading data and model


setwd("Example")

### Getting data
data <- read.csv("exData.csv")

### Getting model
load("model.R")

#### Survival Model

### Running basic
psc <- psc(model,data)


### Running with missing covariate
data_err_1 <- data[,-3]
psc <- psc(fpm,data_err_1)

### Running with missing data (should give warning)
data_err_2 <- data
data_err_2[2:5,2:5] <- NA
psc <- psc(fpm,data_err_2)

### Running with misnamed outcome
data_err_3 <- data
names(data_err_3)[which(names(data_err_3)=="time")] <- "surv_time"
psc <- psc(fpm,data_err_3)



### Error's

# Warning on Simulation size
# Warning on data distribution
  # Continuous outside of range
  # Categ











