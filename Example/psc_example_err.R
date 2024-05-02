### Example of PSC in R


library(devtools)
.rs.restartR()
remove.packages("psc")
devtools::install_github("RichJJackson/psc",ref="devMay24")
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


plot.psc(res)
plot(res)

coef(res)
coef.psc(res)

print(res)
print.psc(res)

summary(res)
summary.psc(res)


## Exares#res## Exares## Example output
plot(res)
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











