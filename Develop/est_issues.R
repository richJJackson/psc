### Estimation Issues



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


CFM <- fpm.tace
DC <- data
nsim <- 500
id <- NULL
trt <- NULL

#pscfit <- function (CFM, DC, nsim = 5000, id = NULL, trt = NULL) {

  ### Cleaning Data
  DC_clean <- dataComb(CFM, DC, id=id, trt = trt)


  DC_clean
  ### Starting Parameters
  init <- initParm(CFM = CFM, DC_clean = DC_clean, trt = trt)

  ### MCMC estimation
  mcmc <- pscEst(CFM = CFM, DC_clean = DC_clean, nsim = nsim,
                 start = init$par, trt = trt)

  ### Formatting results
  covnm <- "beta"
  if (!is.null(trt)) {
    df <- data.frame(DC_clean$cov)
    ft <- factor(df$trt)
    covnm <- paste("beta", levels(ft), sep = "_")
  }

  mcmc <- data.frame(mcmc)
  names(mcmc) <- c(colnames(DC_clean$model_extract$sig), covnm,
                   "DIC")
  psc.ob <- list(model.type = class(CFM), DC_clean = DC_clean,
                 posterior = mcmc)
  class(psc.ob) <- "psc"
  return(psc.ob)
}




### Data match function


cfm.data <- mf
dc.data <- DC







pscCFM







######





