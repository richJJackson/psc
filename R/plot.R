#' Function for Plotting PSC objects
#' @param x an object of class 'psc'
#' @return a plot corresponding to the psc fit
#' @details making use of the generic 'plot' functions this will provide some
#'   graphical output of the fitted psc object.  The form of the output will
#'   depend on the class of the initial model
#' @import ggplot survminer
#' @export
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


