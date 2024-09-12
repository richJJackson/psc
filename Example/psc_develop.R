### Example of PSC in R


#.rs.restartR()
rm(list=ls())
remove.packages("psc")
devtools::install_github("RichJJackson/psc",ref="Version-1.0")
library(psc)



### Example

data(data)
data("surv.mod")

dc <- dataComb(surv.mod,data)
ps <-pscSumm(dc)
surv.psc <- pscfit(surv.mod,data)

print(surv.psc)
coef(surv.psc)
summary(surv.psc)
plot(surv.psc)



data("bin.mod")

dc <- dataComb(bin.mod,data);dc
ps <-pscSumm(dc);
bin.psc <- pscfit(bin.mod,data)

print(bin.psc)
coef(bin.psc)
summary(bin.psc)
plot(bin.psc)

DC_clean$model.type











## Need a function which estimates the median survival for each patient









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










