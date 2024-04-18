### Example of PSC in R

library(devtools)
devtools::install_github("RichJJackson/psc")
library(psc)
library(survival)
library(mvtnorm)

## loading data and model
setwd("Example")

### Getting data
load("exData.R")

### Getting model
load("model.R")

#### Survival Model

### Running basic
psc <- pscfit(model,data)

## Example output
print(psc)
plot(psc)
summary(psc)


plot.psc
