### Data summary





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

data$tumour.number <- factor(data$tumour.number,labels=c("Singular","Multiple"))


data$time <- data$OS_months
data$cen <- data$status

### Fails! - lets see why
pscfit(fpm.tace,data)



########################################
library(ggplot2)
library(gridExtra)

### Visualisations of data in model

cfm <- fpm.tace
pscCFM


### Getting dataset
dset <- cfm$data$m
dset <- dset[,-c(1,ncol(dset))]

## Create numeric/integer and factor.character

cls <- lapply(dset,class)

num.dset[1:3,]

num.dset <- dset[,(cls%in%c("numeric"))]
fac.dset <- dset[,(cls%in%c("factor","character"))]


### num.plot
num.dset[1:3,]

num.nm <- names(num.dset)

i<-1

ggp <- list()
for(i in 1:6){

  nm <- num.nm[i];nm
  x <- num.dset[,i];x

  ggp[[i]] <-ggplot(data=num.dset,aes(x=x)) +
  geom_density(aes(x=x,y=..density..), fill="#69b3a2" ) +
  ggtitle(nm)

}
i
p

num.dset[1:3,]


hist(num.dset[,1])

ggp[[2]]

grid.arrange(ggp[[1]],ggp[[2]],ggp[[3]],ggp[[4]],ggp[[5]],ggp[[6]],nrow=6)

ggp

p1

cls

ncol(dset) == ncol(fac.dset)+ncol(num.dset)
ncol(dset)


## Comparisons of DC to m


