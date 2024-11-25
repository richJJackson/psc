#' Function for Plotting PSC objects
#' @param x an object of class 'psc'
#' @return A plot showing the individual treatment effects
#' @details This function plots the expected response of the control treatment
#'    along with the observe response rates for each patient in the dataset
#' @import ggplot2
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
