### Example of PSC in R

library(devtools)
devtools::install_github("RichJJackson/psc")
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

<<<<<<< HEAD
psc

## Example output
print(res)
plot(res)
=======
>>>>>>> parent of 0e19d15 (updated example)

