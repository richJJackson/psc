#' Function for Plotting PSC objects
#'
#' A function which plots a patients response against their predicted response
#' from a CFM.  Exact form of the output will depend on the form of the glm
#'
#' @param x an object of class 'psc'
#' @return A plot showing the individual treatment effects
#' @details This function plots the expected response of the control treatment
#'    along with the observe response rates for each patient in the dataset
#' @import ggplot2
#' @examples
#' library(psc)
#' library(survival)
#' bin.mod <- psc::bin.mod
#' data <- psc::data
#' bin.psc <- pscfit(bin.mod,data)
#' plot_ite(bin.psc)
#' @export
#'
plot_ite.glm<- function(x){

  # Binding local varaibles
  id <- NULL

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
