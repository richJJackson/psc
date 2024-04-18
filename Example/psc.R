### Example of PSC in R

library(devtools)
devtools::install_github("RichJJackson/psc")
library(psc)
library(survival)
library(mvtnorm)


## loading data and model
setwd("/Users/richardjackson/Documents/local/psc/data")

### Getting data
load("exData.R")

### Getting model
load("model.R")

#### Survival Model

### Running basic
res <- pscfit(model,data)


## Example output
print(res)
plot(res)

### comparing with survival outcome from data (not needed)
s.ob <- Surv(data$time,data$cen)
lines(survfit(s.ob~1))

## summary res
summary(res)
