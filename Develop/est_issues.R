### Estimation Issues


devtools::load_all()

library(survival)

setwd("~/Documents/GitHub/psc/Develop")
devtools::load_all()
### UK COMPASS

data <- read.csv("ukcData.csv")
load("UKCmodel.Rdata")

pscfit(flexspline_model5,data)
devtools::load_all()

## Added a 'silent=T' option to the try wrapper so that 'solved' the problem but
## plenty of 'failures here' - need a closer look
## likelihood was failing for some estimation of f - didn't fix but applied a
## ammendment to the code so it doesn't fail


######## TACTICS

## step 1 - compare data to model

data <- read.csv("TACTICS data final.csv")
model <- load("fpm.tace.R")




### Model Object
### Setting
P <- "Patients undergoing surgery for a complex aortic aneurysm"
I <- "Open Surgery"
C <- "Not Included in Model"
O <- "Overall Survival"
sett <- cfmSett(P,I,C,O)

cfm.ob <- pscCFM(fpm.tace,setting=sett)

cfm.ob$setting


### Function to compare data to modelled data

#pscCompare <- function(cfm.ob,data){

  sett <- cfm.ob$setting
  dlist <- sett$data

  nmm <- names(dlist)
  nmd <-   names(data)

  ## outcome
  if("flexsurvreg"%in%cfm.ob$class){
    out.msg <- "not checked"
    cond <- "time"%in%nmd&"cen"%in%nmd;cond
      if(cond) out.msg <- "time and cen variables both in dataset"
      if(!cond) out.msg <- "WARNING: time or cen variable missing from dataset"
    }



  n.nmm <- length(nmm)
  modnmm <- nmm[1:(n.nmm-2)]
  i <- modnmm[1]

  dataComparison <- list()
  for(i in 1:length(modnmm)){
    cd <- compareData(modnmm[i],dlist,data)
    dataComparison[[i]] <- cd
  }

  dataComparison

  #}

nm
data[1:3,]

nm <- "afp"
nm <- "tumour.number"

compareData(nm,dlist,data)

nm

compareData <- function(nm,dlist,data){

  cond1 <- nm%in%names(dlist);cond1

  if(cond1){
    nmmod <- which(names(dlist)%in%nm)
    x <- dlist[nmmod]
    }

  cond2 <- nm%in%names(data)
  names(data)

  if(cond2){
    nmdata <- which(names(data)%in%nm);nmdata
    y <- data[,nmdata]
  }

  ## Getting class of data
  cls <- x[[1]][1];cls


  ### factor summary
  if("factor"%in%cls){

    dataRet <- "data comparable"

    nmx <- names(x[[1]])[-1]
    nmy <- unique(y)

    if(any(!(nmx%in%nmy)))
      dataRet <- paste("WARNING: Model levels for",nm,"do not match data levels")
    if(any(!(nmy%in%nmx)))
      dataRet <- paste("WARNING: Model levels for",nm,"do not match data levels")

      tbmod <- x[[1]][-1]
      tbdata <- table(y)


      if(dataRet!="data comparable") ret <- dataRet
      if(dataRet=="data comparable") {
        cmp <- rbind(tbmod,tbdata)
        ret <- cbind(nm,cls,c("Model","Data"),cmp)
      }
  }



  ### numeric summary
  if("numeric"%in%cls){

    dataRet <- "data comparable"

    sumod <- x[[1]][-1];sumod
    sudata <- summary(y);sudata
    rsudata <- round(sudata,2)

    mod_iqr <- as.numeric(as.character(sumod[5]))-as.numeric(as.character(sumod[4]))
    dat_iqr <- as.numeric(sudata[5] - sudata[2])
    iqr_check <- mod_iqr/dat_iqr;iqr_check
    iqr_cond <- iqr_check<0.5|iqr_check>2|is.na(iqr_check);iqr_cond


    med_cond1 <- sudata[3] > as.numeric(as.character(sumod[5]))
    med_cond2 <- sudata[3] < as.numeric(as.character(sumod[4]))

    if(iqr_cond|med_cond1|med_cond2){
      dataRet <- "Data seem to be from different distributions"
    }

    r1 <- paste(sumod[3]," (",sumod[5],", ",sumod[5],")",sep="")
    r2 <- paste(rsudata[3]," (",rsudata[2],", ",rsudata[4],")",sep="")

    nbelow <- length(which(y<sumod[1]))
    nabove <- length(which(y>sumod[2]))

    cmp <- rbind(c(r1,"-","-"),c(r2,nbelow,nabove))
    ret <- cbind(nm,cls,c("Model","Data"),cmp,"msg"=dataRet)

    }

 ret
}



##### PSC


dataSumm


#################
## Data cleaning

data$OS_months <- as.numeric(as.Date(data$Date,"%d/%m/%Y")-as.Date(data$Date.of.randomization,"%d/%m/%Y"))/30.44
data$status <- data$Death.1.Alive.0.2020.7.31Cutoff

### defining survival object
data$s.ob <- Surv(data$OS_months,data$status)



### Cleaning Data

### Removing patient with HBV and HCV
data <- data[-which(data$"HBｓ.Ag"=="+"&data$HCV.Ab=="+"),]

data$tumour.number <- cut(data$Number.of.tumors,c(0,1.5,20),c(0,1))
data$tum.siz <- log(data$Maximum.Tumor.size.cm.+0.1,10)
data$afp <- log(as.numeric(data$AFP..ng.ml.)+1,10)
data$alb <- data$ALB..g.dl.*10
data$bil <- log(data$T.Bil..mg.dl.*10,10)
data$vi <- "No"
data$cp <- "Child-Pugh A"

data$hcv <- 0
data$hcv[which(data$HCV.Ab=="+")] <- 1

data$hbv <- 0
data$hbv[which(data$HBｓ.Ag=="+")] <- 1

#data$aetOther[which(data$"HCV.Ab"=="-"&data$"HBｓ.Ag"=="-")] <- 1
data$ecog <- data$ECOG.PS

data$tumour.number <- factor(data$tumour.number,labels=c("Single","Multiple"))


data$time <- data$OS_months
data$cen <- data$status


pscfit(fpm.tace,data)





pscCFM







######





