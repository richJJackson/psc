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
res <- psc(model,data)


## Example output
print(res)
plot(res)
<<<<<<< HEAD

### comparing with survival outcome from data (not needed)
s.ob <- Surv(data$time,data$cen)
lines(survfit(s.ob~1))

## summary res
=======
>>>>>>> parent of 0e19d15 (updated example)
summary(res)
plot.psc



