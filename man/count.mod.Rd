#' Example model for a survival outcome
#'
#' A model of class 'glm'
#'
#' @format A model of class 'flezsurvreg'
#' \describe{
#'  \item{gamma}{cumulative baseline hazard parameters}
#'  \item{vi}{vascular invasion}
#'  \item{age60}{patient age (centred at 60)}
#'  \item{ecog}{ECOG performance Status}
#'  \item{logafp}{AFP - log scale}
#'  \item{alb}{albumin}
#'  \item{{logcreat}{Creatinine - log scale}
#'  \item{allmets}{metastesis}
#'  \item{ageVasInv}{centred age nested within vascular invasion}
#'  \item{time}{survival time}
#'  \item{cen}{censoring indicator}
#'  \item{os}{survival time}
#'  \item{count}{exapmple outcome for count data}
#'  \item{trt}{exapmple identifier for mulitple treatment comparisons}
#'  \item{aet}{Aetiology}
#' }
#' @source simulated
"count.mod"
