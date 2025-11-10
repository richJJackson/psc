#' Function for Plotting PSC objects
#'
#' A function which illsutrates the predicted response under the Counter Factual
#' Model (CFM) and the observed response under the experimental treatment(s).
#' Form of the output will depend on the form of the CFM used
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return a survival plot corresponding to the psc fit
#' @details This function plots the expected response of the control treatment
#'    along with the observe response rates of the experimental arms
#' @import ggplot2 ggpubr
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' psc <- pscfit(gemCFM,e4_data)
#' plot(psc)
#' @export
plot.psc <- function (pscOb, ...){

  p <- NULL
  model.type <- pscOb$mod_class;model.type
  fam <- pscOb$family;fam

  if ("glm" %in% model.type) {

    ### gaussian
    if(fam$family%in%c("gaussian")){
      p <- plot.psc.cont(pscOb)
    }
    ## poisson
    if(fam$family%in%c("poisson")){
      p <- plot.psc.count(pscOb)
    }
    ## binomial
    if(fam$family%in%c("binomial")){
      p <- plot.psc.binary(pscOb)
    }
  }

  if ("flexsurvreg" %in% model.type) {
    p <- plot.psc.flexsurvreg(pscOb)
  }
  p
}

