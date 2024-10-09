### Example of PSC in R


#.rs.restartR()
rm(list=ls())
remove.packages("psc")
devtools::install_github("RichJJackson/psc",ref="Version-1.0")
library(psc)



### Example

data(data)
data("surv.mod")


surv.mod

dc <- dataComb(surv.mod,data)
ps <-pscSumm(dc)
surv.psc <- pscfit(surv.mod,data)

print(surv.psc)
coef(surv.psc)
summary(surv.psc)
plot(surv.psc)

data[1:3,]

pc.mtc <- pscfit(surv.mod,data,trt=data$trt)



DC_clean <- pc.mtc$DC_clean
summary(pc.mtc)

pscSumm <- function (DC_clean)
{

  lp <- linPred(DC_clean, resp = F)
  resp <- linPred(DC_clean, resp = T)
  if ("glm" %in% DC_clean$model.type) {
    exp_resp <- mean(resp)
    ob_resp <- mean(unlist(DC_clean$out))
  }
  if ("flexsurvreg" %in% DC_clean$model.type) {
    s.ob <- Surv(DC_clean$out$time, DC_clean$out$cen)
    sfit <- survfit(s.ob ~ 1)
    sfit.tab <- summary(sfit)$table
    exp_resp <- median(resp)
    ob_resp <- sfit.tab[7]
  }
  cat(paste(nrow(DC_clean$cov), "observations selected from the data cohort for comparison"),
      "\n")
  cat("CFM of type", DC_clean$model.type, "identified", " \n")
  cat(paste("linear predictor succesfully obtained with a median of ",
            round(median(lp), 3)), "\n")
  cat(paste("Average expected response:", round(exp_resp, 3)),
      "\n")
  cat(paste("Average observed response:", round(ob_resp, 3)),
      "\n")
  ret <- list(linpred = lp, `E(patResp)` = resp, expResp = exp_resp,
              obResp = ob_resp)
  return(ret)
}






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










