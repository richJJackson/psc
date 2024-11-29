#' Function for Plotting PSC objects
#'
#' A function which plots a patients response against their predicted response
#' from a CFM
#'
#' @param x an object of class 'psc'
#' @return A plot showing the individual treatment effects
#' @details This function plots the expected response of the control treatment
#'    along with the observe response rates for each patient in the dataset
#' @import ggplot2
#' @examples
#' library(psc)
#' library(survival)
#' surv.mod <- psc::surv.mod
#' data <- psc::data
#' surv.psc <- pscfit(surv.mod,data)
#' plot_ite(surv.psc)
#' @export
plot_ite.flexsurvreg<- function(x){

  # Binding local varaibles
  ggdata <- S <- lresp <- id <- ltime <- NULL

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
