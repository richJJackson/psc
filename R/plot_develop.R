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

plot(pssur)
plot.psc.flexsurvreg(pssur)


plot.psc.flexsurvreg <- function(x){
  med <- coef(x)
  med <- med[-nrow(med),1]

  ### Getting model survival estimate
  s_fpm <- surv_fpm(x$DC_clean)
  s_data <- data.frame("time"=s_fpm$time,"S"=s_fpm$S)

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
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

  pr_data
  ggplot(aes(pr_cont,colour=Outcome),data=pr_data)+
    geom_density(linewidth=1.3)+
    xlim(c(0,1)) +
    xlab("Pr(Response)")+
    ylab("Density") +
    geom_vline(aes(xintercept=mean(pr_cont),colour=Outcome),linetype="dashed",linewidth=1.2)+
    geom_vline(xintercept=new.mn,col=c(3:(length(new.mn)+2)),linewidth=1.2)

}
