### Muliple Treatment Comparisons


library(psc)
library(survival)
library(enrichwith)


data <- psc::data
surv.mod <- psc::surv.mod
bin.mod <- psc::bin.mod
cont.mod <- psc::cont.mod
count.mod <- psc::count.mod


pssur <- pscfit(surv.mod,data,trt=data$trt)
pssur_null <- pscfit(surv.mod,data)
psbin <- pscfit(bin.mod,data,trt=data$trt)
psbin_null <- pscfit(bin.mod,data)
pscon <- pscfit(cont.mod,data,trt=data$trt)
pscou <- pscfit(count.mod,data,trt=data$trt)


summary(pssur)
summary(pssur_null)
summary(psbin)
summary(pscou)

print(pssur)
print(psbin)
print(pscon)
print(pscou)

plot(pssur)
plot(psbin)
plot(pscon)
plot(pscou)

DC_clean <- pssur$DC_clean

pscSumm(DC_clean)

pscSumm <- function(DC_clean){

  lp <- linPred(DC_clean,resp=F)
  resp <- linPred(DC_clean,resp=T)
  mtc.cond <- "trt"%in%colnames(DC_clean$cov)
  trt <- rep(1,nrow(DC_clean$cov))
  if(mtc.cond) trt <- factor(DC_clean$cov[,which(colnames(DC_clean$cov)=="trt")])
  lev <- levels(trt)


  lp_ret <- tapply(lp,trt,median)
  DC_clean$cov

  if("glm" %in% DC_clean$model.type){
    #exp_resp <- mean(resp)
    exp_resp <- tapply(resp,trt,median)
    ob_resp <- mean(unlist(DC_clean$out))
  }


  if("flexsurvreg" %in% DC_clean$model.type){
    s.ob <- Surv(DC_clean$out$time,DC_clean$out$cen)
    sfit <- survfit(s.ob~1)
    sfit.tab <- summary(sfit)$table


    #exp_resp <- median(resp)
    exp_resp <- tapply(resp,trt,median)
    ob_resp <- sfit.tab[7]
  }

  cat(paste(nrow(DC_clean$cov),"observations selected from the data cohort for comparison"),"\n")
  cat("CFM of type",DC_clean$model.type,"identified"," \n")
  cat("linear predictor succesfully obtained with median: \n ")
  cat(paste("trt",lev,": ",round(lp_ret,3),"\n",sep=""))
  cat("Average expected response: \n ")
  cat(paste("trt",lev,": ",round(exp_resp,3),"\n",sep=""))
  cat(paste("Average observed response:",round(ob_resp,3)),"\n")

  ret <- list("linpred"=lp,"E(patResp)"=resp,"expResp"=exp_resp,"obResp"=ob_resp)
  return(ret)
}




linPred <- function(DC_clean,resp=F){

  mt <- DC_clean$model.type
  cov <- DC_clean$cov;cov

  ## Removing 'trt' from dataset
  if("trt"%in%colnames(cov)){
    cov <- cov[,-which(colnames(cov)=="trt")]
  }

  cov_co <- DC_clean$model_extract$cov_co;cov_co
  lp <- cov %*% cov_co
  ret <- lp

  if(resp){
    if("glm"%in%mt){
      fam <- enrich(DC_clean$model_extract$family)
      ret <- fam$linkinv(lp)
    }

    if("flexsurvreg"%in%mt){
      mnlp <- mean(lp);mnlp
      ret <- surv_fpm(DC_clean,s=0.5)^exp(mnlp-lp);ret
    }
  }
  return(ret)
}



summary.psc <- function(object,...){

  cat("Summary: \n \n")
  summ <-pscSumm(object$DC_clean)

  cat("\n")
  print(object)

}

