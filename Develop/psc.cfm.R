### Creatiung of PSC model object


############# Testing

## loading packages
library(flexsurv)
library(psc)

## Getting Data to create model on
load("/Volumes/richj23/Projects/Cancer/Billiary Tract/Data/btData.R")


###
data$stime <- as.numeric(data$censDt-data$CTstart)/30.44

data <- data[-which(data$stime<=0|is.na(data$stime)),]
data <- data[which(data$Trt=="GemCis"),]

cfm <- flexsurvspline(Surv(stime,dead)~extent+primary+alb+bil,data=data,k=3)

covnm <- c("Extent of Resection","Primary Disease","Albumin","Bilirubin")

sett <- cfmSett(
  P="Patients with Billiary Tract Cancer undergoing first line adjuvant therapy",
  C="Gemcitabine Monotherapy",
  O="Overall Survival measured as the time from first therapy adminstration until death by any cause"
)

int.valid <- cfmValid(cfm)

citation <- "Example Citation"

cfm.ob <- pscCFM(cfm,covnm=covnm,setting=sett)



setwd("~/Documents/local/psc/Develop")
save(cfm.ob,file="cfm.ob.R")


#################################
#### Functions












