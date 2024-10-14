#' Fucntion for Plotting PSC objects
#' @param x an object of class 'psc'
#' @param ... not used
#' @return a plot corresponding to the psc fit
#' @details making use of the generic 'plot' functions this will provide some
#'   graphical output of the fitted psc object.  The form of the output will
#'   depend on the class of the initial model
#' @import graphics
#' @export
plot.psc <- function (x, ...){

  model.type <- x$'model.type';model.type

  if ("glm" %in% model.type) {

    fam <- x$DC_clean$model_extract$family;fam
    out <- as.numeric(unlist(x$DC_clean$out));out
    newr <- mean(out)

    cov <- x$DC_clean$cov;cov
    cov_co <- x$DC_clean$model_extract$cov_co;cov_co
    lp <- cov %*% cov_co
    pr_cont <- fam$linkinv(lp)

    xmax <- max(c(pr_cont,out))/0.9
    xmin <- min(c(pr_cont,out))*0.9
    xl <- c(xmin,xmax)
    den <- density(pr_cont)

    ymax <- max(den$y)/0.7

    ### gaussian
    if(fam$family%in%c("gaussian")){
      denn <- density(out)
      xmax <- max(xmax,max(out))
      xmin <- min(xmin,min(out))
      xl <- c(xmin,xmax)
      ymax <- max(ymax,denn$y)

      plot(den,col="royalblue",xlim=xl,ylim=c(0,ymax),lwd=4,xlab="Response")
      lines(denn,col="mistyrose",lwd=4)
      abline(v=newr,col="darkred",lty=2,lwd=3)
      legend(xmin*1.1,ymax*.975,c("Predicted Control Response","Observed response","Observed Mean"),bty="n",col=c("royalblue","mistyrose","darkred"),lwd=4,lty=c(1,1,2))
    }


    ## poisson
    if(fam$family%in%c("poisson")){
      plot(table(out),xlim=xl,col="mistyrose",lwd=15,pch=15)
      par(new=T)
      plot(density(pr_cont),xlim=xl,lwd=6,col="royalblue",xlab="Pr (Response)",main="")
      legend(xmin*1.1,ymax*.975,c("Predicted Control Response","observed response"),bty="n",col=c("royalblue","darkred"),lwd=c(3,3),lty=c(1,2))
    }

    ## binomial
    if(fam$family%in%c("binomial")){
      plot(density(pr_cont),xlim=xl,lwd=6,col="royalblue",xlab="Pr (Response)",main="")
      abline(v=newr,col="darkred",lty=2,lwd=3)
      legend(xmin*1.1,ymax*.975,c("Predicted Control Response","observed response"),bty="n",col=c("royalblue","darkred"),lwd=c(3,3),lty=c(1,2))
    }


  }

  if ("flexsurvreg" %in% model.type) {
    med <- coef(x)
    med <- med[-nrow(med),1]

    cls <- c("royalblue","pink","darkorchid","black","darkorange","forestgreen")

    s_fpm <- surv_fpm(x$DC_clean)
    plot(s_fpm$time, s_fpm$S, typ = "l", col = cls[1], lwd = 4,
         ylab = "Overall Survival", xlab = "Time")

    for(i in 1:length(med)){
      s_fpm_2 <- surv_fpm(x$DC_clean, beta = med[i])
      lines(s_fpm_2$time, s_fpm_2$S, typ = "l", col = cls[(i+1)], lwd = 4)
    }
  }

}

