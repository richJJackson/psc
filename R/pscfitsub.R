
pscfitsub <- function(CFM,DC,sg,cont_lev=2,cont_cut=NULL){


  if(class(sg)%in%c("character","factor")){
    sg.id <- factor(sg)
    lev <- levels(sg)
  }

  if(class(sg)%in%c("numeric","integer")){
    if(is.null(cont_cut)){
      sg.id <- cut(order(sg),breaks=cont_lev)
    }
    if(!is.null(cont_cut)){
      sg.id <- cut(sg,c(-Inf,cont_cut,Inf))
    }
    lev <- levels(sg.id)
  }

  nlev <- length(lev)
  tb <- table(sg.id,dc$cen);tb

  fit <- list()

  for(i in 1:nlev){
    id <- which(sg.id%in%lev[i]);id
    fit[[i]] <- pscfit(CFM,DC,id=id)
  }

  ret <- list("Table"=tb,"fit"=fit)
  return(ret)

}
