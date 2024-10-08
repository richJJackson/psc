#' Fucntion for Plotting PSC objects
#' @param x an object of class 'psc'
#' @return a plot corresponding to the psc fit
#' @details making use of the generic 'plot' functions this will provide some
#'   graphical output of the fitted psc object.  The form of the output will
#'   depend on the class of the initial model
#' @examples
#' psc.ob <- psc(model,data)
#' plot(psc.ob)
#' @export
plot.psc <- function (x, ...){

  model.type <- x$'model.type'


  if ("glm" %in% model.type) {
    pr.new <- predict(CFM,type="response",newdata=DC)
    pr.old <- predict(CFM,type="response")
    den.new <- density(pr.new)
    den.old <- density(pr.old)

    ymax <- max(den.new$y,den.old$y)
    tb <-  table(DC$out)
    newr <- tb[2]/sum(tb)
    xmax <- max(den.new$x,den.old$x,newr*1.1)
    xmax <- min(c(xmax,1))

    plot(den.old,xlim=c(0,xmax),ylim=c(0,ymax),lwd=3,col="royalblue",xlab="Pr (Response)",main="",...)
    lines(den.new,col="pink",lwd=3)
    abline(v=newr,col="darkred",lty=2,lwd=3)

    legend(xmax*.4,ymax*.9,c("Pr(Resp.): control","Pr(Resp.): experimental","observed response"),bty="n",col=c(4,6,2),lwd=c(3,3,3),lty=c(1,1,2))
  }

  if ("flexsurvreg" %in% model.type) {
    med <- coef(x)
    med <- med[-nrow(med),1]


    cls <- c("royalblue","pink","darkorchid","black","darkorange","forestgreen")

    s_fpm <- surv_fpm(x)
    plot(s_fpm$time, s_fpm$S, typ = "l", col = cls[1], lwd = 4,
         ylab = "Overall Survival", xlab = "Time")

    for(i in 1:length(med)){
      s_fpm_2 <- surv_fpm(x, beta = med[i])
      lines(s_fpm_2$time, s_fpm_2$S, typ = "l", col = cls[(i+1)], lwd = 4)
    }
  }

}


