#' Function for Plotting PSC objects
#' @param x an object of class 'psc'
#' @return a survival plot corresponding to the psc fit
#' @details making use of 'ggsurvplot' in the survminer package, this function
#' plots the expected survival funtion for the 'control' treatment estimated
#' from the CFM along with the Kaplan Meier estimates of the observed events
#' @import ggplot survminer
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

