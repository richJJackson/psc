### Example of PSC in R

library(devtools)
install_github("RichJJackson/psc",ref="develop",force=T)
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
res <- pscfit(model,data)

## Example output
plot(res)
summary(res)

