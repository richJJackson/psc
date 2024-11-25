#' Function for Plotting individual treatment effects for a PSC object
#' @param x an object of class 'psc'
#' @return a plot corresponding to the psc fit
#' @details a funtion which returns a plot showing the individual estimated
#' response under the control treatment (from the CFM) against the observed
#' response from the data cohort
#' @import graphics
#' @export
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
