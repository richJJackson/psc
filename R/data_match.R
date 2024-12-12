#' A function to ensure that data from the cfm and data cohort are compatible
#'
#' The purpose of this function is to run a series of checks to ensure that the
#' data included in the data cohort is comparable to the counter-factual model
#'
#' @param cfm.data a model object supplied to pscfit
#' @param dc.data a dataset including covariates to match the CFM
#' @return a dataset which is checked and compatible with the CFM
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
  nm <- names(cfm.data)

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
