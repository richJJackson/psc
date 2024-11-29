#' Function for Plotting individual treatment effects for a PSC object
#'
#' A function which plots a patients response against their predicted response
#' from a CFM.  Exact form of the output will depend on the form of the model supplied
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
