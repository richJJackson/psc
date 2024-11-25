### Develop plotting output


library(psc)
library(survival)
library(enrichwith)
library(ggplot2)
library(survminer)

data <- psc::data
surv.mod <- psc::surv.mod
bin.mod <- psc::bin.mod
cont.mod <- psc::cont.mod
count.mod <- psc::count.mod


pssur <- pscfit(surv.mod,data,trt=data$trt)
psbin <- pscfit(bin.mod,data,trt=data$trt)
pscou <- pscfit(count.mod,data,trt=data$trt)
pscon <- pscfit(cont.mod,data,trt=data$trt)


plot(pssur)
plot(psbin)
plot(pscou)
plot(pscon)

print(psbin)
coef(psbin)
summary(psbin)

plot_ite(pssur)
plot_ite(psbin)
plot_ite(pscou)
plot_ite(pscon)



plot.psc.flexsurvreg <- function(x){
  med <- coef(x)
  med <- med[-nrow(med),1]

  ### Getting model survival estimate
  s_fpm <- surv_fpm(x$DC_clean)
  s_data <- data.frame("time"=s_fpm$time,"S"=s_fpm$S)

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov);mtc.cond
  trt <- rep(1,nrow(x$DC_clean$cov));trt
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])

  # plot
  out <- x$DC_clean$outcome
  sfit <- survfit(Surv(time,cen)~trt,data=out)
  sfit_plot <- ggsurvplot(sfit,data=out)$plot
  sfit_plot + geom_line(data=s_data, aes(time,S),col=6,lwd=1.5)

}




plot.psc.binary <- function(x){

  fam <- x$DC_clean$model_extract$family;fam
  out <- as.numeric(unlist(x$DC_clean$out));out

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])

  new.mn <- tapply(out,trt,mean)
  length(new.mn)

  pr_cont <- linPred(x$DC_clean,resp=T)
  pr_data <- data.frame(pr_cont)
  pr_data$Outcome <-"CFM"

  ggplot(aes(pr_cont,colour=Outcome),data=pr_data)+
    geom_density(linewidth=1.3)+
    xlim(c(0,1)) +
    xlab("Pr(Response)")+
    ylab("Density") +
    geom_vline(aes(xintercept=mean(pr_cont),colour=Outcome),linetype="dashed",linewidth=1.2)+
    geom_vline(xintercept=new.mn,col=c(3:(length(new.mn)+2)),linewidth=1.2)

}



plot.psc.count <- function(x){

  fam <- x$DC_clean$model_extract$family;fam
  out <- as.numeric(unlist(x$DC_clean$out));out

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])

  new.mn <- tapply(out,trt,mean)

  pr_cont <- linPred(x$DC_clean,resp=T)
  pr_data <- data.frame(pr_cont)
  pr_data$Outcome <-"CFM"

  den <- density(pr_data$pr_cont)
  den <- data.frame("x"=den$x,"y"=den$y)
  infl <- max(table(out))/max(den$y)
  den$y <- den$y*infl

  ggplot(aes(out),data=data.frame(out))+
    geom_bar(col="gray",fill="gray")+
    geom_line(aes(x,y),data=den,linewidth=1.5,col="darkorchid")+
    xlab("Response/Pr(Response)")+
    ylab("Frequency") +
    geom_vline(aes(xintercept=mean(pr_cont),colour=Outcome),data=pr_data,linetype="dashed",linewidth=1.2) +
    geom_vline(xintercept=new.mn,col=c(3:(length(new.mn)+2)),linewidth=1.2)

}




plot.psc.cont <- function(x){

  fam <- x$DC_clean$model_extract$family;fam
  out <- as.numeric(unlist(x$DC_clean$out));out

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])

  new.mn <- tapply(out,trt,mean)

  pr_cont <- linPred(x$DC_clean,resp=T)
  pr_data <- data.frame(pr_cont)
  pr_data$Outcome <-"CFM"

  pr_data
  den <- density(pr_data$pr_cont)
  den.out <- density(out)
  den <- data.frame("x"=den$x,"y"=den$y,"xn"=den.out$x,"yn"=den.out$y)

  ggplot(aes(x,y),data=data.frame(den))+
    geom_line(linewidth=1.5,col=2)+
    geom_line(aes(xn,yn),color="darkorchid",linewidth=1.5)+
    xlab("Response")+
    ylab("Frequency") +

    geom_vline(aes(xintercept=mean(pr_cont),colour=Outcome),data=pr_data,linetype="dashed",linewidth=1.2) +
    geom_vline(xintercept=new.mn,col=c(3:(length(new.mn)+2)),linewidth=1.2)

}




plot_ite.flexsurvreg<- function(x){

  ### Getting model survival estimate
  s_fpm <- surv_fpm(x$DC_clean)
  s_data <- data.frame("time"=s_fpm$time,"S"=s_fpm$S)

  resp <- linPred(x$DC_clean,resp=T)
  out <- x$DC_clean$out;out

  ggdata <- cbind(resp,out)
  ggdata <- ggdata[order(ggdata$resp),]
  ggdata$id <- 1:nrow(ggdata)

  ggdata$lresp <- log(ggdata$resp)
  ggdata$ltime <- log(ggdata$time)

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov);mtc.cond
  trt <- rep(1,nrow(x$DC_clean$cov));trt
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])


  ggplot(aes(lresp,id),data=ggdata)+
    geom_point()+
    geom_segment(aes(lresp,id,xend=ltime,colour=trt),linetype=(2-ggdata$cen))+
    xlab("(log) Time")+
    ylab("ID")

}


plot_ite.glm<- function(x){

  fam <- x$DC_clean$model_extract$family;fam
  out <- as.numeric(unlist(x$DC_clean$out));out

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])

  pr_cont <- linPred(x$DC_clean,resp=T)
  ggdata <- data.frame(pr_cont,out)

  ggdata <- ggdata[order(ggdata$pr_cont),]
  ggdata$id <- 1:nrow(ggdata)

  ggplot(aes(pr_cont,id),data=ggdata)+
    geom_point()+
    geom_segment(aes(pr_cont,id,xend=out,colour=trt))+
    xlab("Pr(Response)")+
    ylab("ID")

}


plot.psc <- function (x){

  model.type <- x$'model.type';model.type
  fam <- x$DC_clean$model_extract$family;fam

  if ("glm" %in% model.type) {
    ### gaussian
    if(fam$family%in%c("gaussian")){
      p <- plot.psc.cont(x)
      }
    ## poisson
    if(fam$family%in%c("poisson")){
      p <- plot.psc.count(x)
      }
    ## binomial
    if(fam$family%in%c("binomial")){
      p <- plot.psc.binary(x)
     }
  }

  if ("flexsurvreg" %in% model.type) {
    p <- plot.psc.flexsurvreg(x)
  }
  p
}


plot_ite <- function (x){

  model.type <- x$'model.type';model.type

  if ("glm" %in% model.type) {
    p <- plot_ite.glm(x)
  }

  if ("flexsurvreg" %in% model.type) {
    p <- plot_ite.flexsurvreg(x)
  }
  p
}





