#' sub group analyses using PSC
#'
#' @param CFM CFM
#' @param DC DC
#' @param sg sg
#' @param cont_lev cont_lev
#' @param cont_cut cont_cut
#'
#' @export
pscsub <- function(CFM,DC,sg,cont_lev=2,cont_cut=NULL){

  pscsub <- pscfitsub(CFM,DC,sg,cont_lev=2,cont_cut=NULL)
  pscsubWrap(pscsub)

}





