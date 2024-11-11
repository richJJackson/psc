### Creatiung of PSC model object


## Getting Data
load("/Volumes/richj23/Projects/Cancer/Billiary Tract/Data/btData.R")

library(flexsurv)
library(psc)

data$stime <- as.numeric(data$censDt-data$CTstart)/30.44

data <- data[-which(data$stime<=0|is.na(data$stime)),]
data <- data[which(data$Trt=="GemCis"),]
data[1:3,]

cfm <- flexsurvspline(Surv(stime,dead)~extent+primary+alb+bil,data=data,k=3)

psc.cfm <- psc.cfm(cfm)





attributes(psc.cfm)




covnm <- c("Extent of Resection","Primary Disease","Albumin","Bilirubin")


psc.cfm <- function(cfm,setting=NULL,covnm=NULL,valid=NULL){


  # Class
  cfm.class <- class(cfm)
  ret <- list()
  ret$class <- cfm.class

  # Setting - Details of where data are taken from and a summary of the data
  mm <- model.matrix(cfm)
  dset <- cfm$data$m

  cfm.data <- lapply(dset[,-c(1,ncol(dset))],dataSumm)
  cfm.data$modnames <- names(cfm.data)
  cfm.data$varnames <- names(cfm.data)

  names(cfm.data)
  if(!is.null(covnm)){
    cfm.data$varnames <- covnm
  }

  ret$data <- cfm.data



  # Components
  ret$mu <- coef(cfm)
  ret$vcov <- vcov(cfm)


  attributes(cfm)
  if(cfm.class=="flexsurvreg"){

    ret$k <- cfm$k
    ret$knots <- cfm$knots
    ret$scale <- cfm$scale

  }


  # Validation
  ret$valid <- cfm.valid.flexsurvreg(cfm)
  class(ret) <- "psc.cfm"

  ret

}






dataSumm <- function(x){

  cl.x <- class(x)

  if(cl.x%in%c("character","factor")){
    x <- factor(x)
    lev <- levels(x)
    tb <- table(x)
    ret <- c("Class"=cl.x,tb)
    }


  if(cl.x%in%c("integer","numeric")){
    x <- as.numeric(x)
    quant <- round(quantile(x,c(0.5,0.25,0.75)),2)
    minx <- min(x,na.rm=T)
    maxx <- max(x,na.rm=T)
    miqr <- paste(quant[1]," (",quant[2],", ",quant[3],")",sep="")
    ret <- c("Class"=cl.x,"min"=minx,"max"=maxx,"miqr"=miqr)
    ret
      }

  ret

}



cfm.valid.flexsurvreg <- function(cfm,exData=NULL,plot=F){

  me <- modelExtract.flexsurvreg(cfm)
  cov <- model.matrix(cfm)
  lp <- t(me$cov_co%*%t(cov))

  ## Internal Validation
  pred_quant <- quantile(lp,c(0.15,0.5,0.85))
  pred_grp <- cut(lp,c(-Inf,pred_quant,Inf),labels=c("risk_grp_1","risk_grp_2","risk_grp_3","risk_grp_4"))

  ##
  out <- cfm$data$m[,1]
  sfit <- survfit(out~pred_grp)
  cm <- coxph(out~pred_grp)

  ### concordance
  c <- summary(cm)$concordance
  lp_slope <- coxph(out~lp)

  if(plot){
    plot(sfit,col=c(1,2,3,4),lwd=4,xlab="Time",ylab="Survival Prob.")
    haz <- fpm_haz(cfm)

    mn.lp <- tapply(lp,pred_grp,median)
    S <- t(exp(-exp(mn.lp)%*%t(haz$H)))

    for(j in 1:ncol(S)){
        lines(haz$time,S[,j],col=j,lwd=2,lty=3)
      }
    }

  ret <- list("km_est"=sfit,"coxph"=cm,"concordance"=c,lp_slope=lp_slope)
  ret
  }



fpm_haz <- function(fpm,tm=c(0:60)){

  time <- tm
  logt <- log(time+1e-06)
  me <- modelExtract.flexsurvreg(fpm)

  lam <- me$lam
  kn <- me$kn
  haz_co <- me$haz_co
  k <- me$k

  ### basis functions
  z <- NULL
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
  }

  H <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  ret <- data.frame("time"=time,"H"=H);ret
  ret
}



modelExtract.flexsurvreg <- function(CFM){
  co <- CFM$coefficients
  k <- CFM$k
  kn <- CFM$knots
  sig <- vcov(CFM)
  max(kn)

  lam <- (max(kn)-kn)/(max(kn)-min(kn))

  form <- formula(CFM)
  mf <- model.frame(CFM)

  n_haz_co <- k+2
  haz_co <- co[1:n_haz_co]
  cov_co <- co[(n_haz_co+1):length(co)]

  ret <- list("model.frame"=mf,"cov_co"=cov_co,"sig"=sig,"haz_co"=haz_co,"k"=k,"kn"=kn,"lam"=lam,"formula"=form)
  return(ret)
}


