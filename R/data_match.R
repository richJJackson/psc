
data_match <- function(cfm.data,dc.data){

  ### duplicated namse in dc.data
  dup <- duplicated(names(dc.data))
  if(any(dup)) dc.data <- dc.data[,-which(dup)]

  ## removing 'weights' column if there
  wid <- which(names(cfm.data)=="(weights)")
  if(length(wid)>0) cfm.data <- cfm.data[,-wid]

  # creating output
  dc.new <- dc.data

  ## Getting Classes
  cls  <- lapply(cfm.data,class);cls
  i<-2
  for(i in 1:ncol(cfm.data)){

    con <- which(names(dc.data)%in%nm[i]);con
    if(length(con)==0) stop("DC missing covariate included in CFM")

    old <-dc.data[,con];old
    new <- old


    if(cls[[i]]%in%c("character","factor")){
      f <- factor(cfm.data[,i]);f
      new <- factor(old);new
      if(!any(levels(new)%in%levels(f))) stop(paste("Factor levels in",nm[i],"not
                                                 represented in model"))
      attributes(new) <- attributes(f)
    }

    if(cls[[i]]%in%c("numeric","integer")){
      new <- as.numeric(as.character(old))
    }

    dc.new[,con] <- new
  }

  ret <- dc.new[,which(names(dc.new)%in%names(cfm.data))]
  ret
}
