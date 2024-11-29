#' Validation of a flexsurvreg model
#' This function intends to perform basic validation of a survival model of class
#' 'flexsurvreg'.  As a default internal validation will be performed by estimating
#' basic measures of discrimination and calibration. If provided, measures will
#' be estimated on external data. This is intended to be used in conjunction
#' with the pscCRM.R function#' A generic function for extracting model information
#' @param cfm a model of class 'flexsurvreg'
#' @param exData an external dataset (for external validation)
#' @return a list of validation procedures for measures of discrimination and
#'     calibration.  Discrimination performed by creating a variable with risk
#'     categories based on the 15th, 50th and 85th percentile of the linear
#'     predictor.  Discrimation estimated provided in terms of kaplan merie
#'     estimates and hazard ratios.
#'
#'     Calibration performed by extracting the concordance statistic and by
#'     regression the linear predictor against the outcome.
#'
#'     Validation is internal unless an external dataset is provided.
#' @export
cfmValid.flexsurvreg <- function(cfm,exData=NULL){

  ret <- list()
  ret$detail <- list
  ret$discrim <- list()
  ret$calib <- list()

  ## Detail
  ret$detail <- "Internal validation based on data used to train model"

  if(!is.null(exData)){
    ret$detial <- "External validation based on dataset other than that used to train the model"
  }


  ## Discrimination
  me <- modelExtract.flexsurvreg(cfm)
  cov <- model.matrix(cfm)

  ###
  ##
  # Add in external data here
  ##
  ###

  lp <- t(me$cov_co%*%t(cov))

  ## Creating groups
  pred_quant <- quantile(lp,c(0.15,0.5,0.85))
  pred_grp <- cut(lp,c(-Inf,pred_quant,Inf),labels=c("risk_grp_1","risk_grp_2","risk_grp_3","risk_grp_4"))

  ##
  out <- cfm$data$m[,1]
  sfit <- survfit(out~pred_grp)
  cm <- coxph(out~pred_grp)

  ret$discrim$sfit <- sfit
  ret$discrim$cm <- cm

  ## Calibration

  # concordance & slope
  c <- summary(cm)$concordance
  lp_slope <- coxph(out~lp)

  ret$calib$c <- c
  ret$calib$slope <- lp_slope

  ## returning object
  ret
}


