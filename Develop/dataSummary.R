### Data summary

library(ggplot2)
library(gridExtra)
library(waffle)



### Getting example - TACTICS

setwd("~/Documents/GitHub/psc/Develop")
devtools::load_all()

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
cfm.ob

##### PSC


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

data$tumour.number <- factor(data$tumour.number,labels=c("Solitary","Multiple"))


data$time <- data$OS_months
data$cen <- data$status

### Fails! - lets see why
pscfit(fpm.tace,data)



########################################


### Visualisations of data in model

cfm <- fpm.tace
pscCFM

### Getting dataset
dset <- cfm$data$m
dset <- dset[,-c(1,ncol(dset))]

## Create numeric/integer and factor.character

cls <- lapply(dset,class)

num.vis <- function(x,nm){
  df <- data.frame("y"=x)
  p <- ggplot(aes(x=y),data=df) +
    geom_density(aes(x=y,y=..density..), fill="#69b3a2", colour="#69b3a2" ) +
    xlab("")+
    ylab("")+
    ggtitle(nm)+
    theme_void()+
    theme(plot.title = element_text(hjust = 0.05))
  p
}


fac.vis <- function(x,nm){
  db <- data.frame(table(x));db
  p <- ggplot(data=db,aes(fill=x,values=Freq))+
    geom_waffle(color="white",size=0.33,n_rows=10)+
    theme_void()+
    scale_fill_manual(values = c("#69b9f2", "#69b9a2", "#69b9c2")) +
    ggtitle(nm)+
    theme(legend.position="right") +
    theme(legend.title=element_blank())+
    theme(plot.title = element_text(hjust = 0.07))
  p
}



gglist <- list()
for(i in 1:ncol(dset)){

  cls[i]
  x <- dset[,i]
  nm <- names(dset)[i]
  if(cls[i]%in%c("factor","character")){
    gglist[[i]] <- fac.vis(x,nm)
  }

  if(cls[i]%in%c("numeric","integer")){
    gglist[[i]] <- num.vis(x,nm)
  }

}


grid.arrange(grobs=gglist,ncol=2)

#############################################

num.vis.compare <- function(p,x){
  dnew <- data.frame("xnew"=x)
  p + geom_density(aes(x=xnew,y=-..density..),data=dnew, fill="#404080",color="#404080" )
}


fac.vis.compare <- function(p,x){

  old.d <- p$data
  tit <- p$labels$title
  old.d$source="CFM"
  dbnew <- data.frame(table(x));dbnew
  dbnew$source <- "DC"
  df <- rbind(old.d,dbnew)

  p <- ggplot(data=df,aes(fill=x,values=Freq))+
    geom_waffle(color="white",size=0.33,n_rows=10)+
    theme_void()+
    facet_wrap(~source,ncol=1)+
    scale_fill_manual(values = c("#69b9a2","#404080","red","blue")) +
    ggtitle(tit)+
    theme(legend.position="right") +
    theme(legend.title=element_blank())+
    theme(plot.title = element_text(hjust = 0.07))
  p
  }



## Comparisons of DC to m
gglist.compare <- list()
for(i in 1:ncol(dset)){

  x <- data[,which(names(data)%in%names(dset)[i])]

  if(cls[i]%in%c("factor","character")){
    if(!any(unique(dset[,1])%in%unique(x))) warning("factor levels do not match")
    gglist.compare[[i]] <- fac.vis.compare(gglist[[i]],x)
  }

  if(cls[i]%in%c("numeric","integer")){
    gglist.compare[[i]] <- num.vis.compare(gglist[[i]],x)
  }

}



grid.arrange(grobs=gglist.compare,ncol=2)






dataSumm.num <- function(x){
  x <- as.numeric(x)
  quant <- round(quantile(x,c(0.5,0.25,0.75)),2)
  minx <- min(x,na.rm=T)
  maxx <- max(x,na.rm=T)
  miqr <- paste(quant[1]," (",quant[2],", ",quant[3],")",sep="")
  ret <- c("min"=minx,"max"=maxx,"med"=quant[1],"low"=quant[2],
           "upp"=quant[3])
  ret
}

dataSumm.fac <- function(x){
  x <- factor(x)
  lev <- levels(x)
  tb <- table(x)
  ret <- c(tb)
}







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


