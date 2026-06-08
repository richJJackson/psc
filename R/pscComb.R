#' PSC Estiamtion combination
#'
#' This function allows the combination of estimation where mulitple treatment
#' comparisons are being made and one of the treatment levels relates to the
#' control level.  Note here it is assumed that the control level is coded
#' trt=0. This function then combines the two efficacy estimates for the
#' treatment effect (concurrent and synhtetic) whilst accounting for the
#' correlation inherent in these two estimates.
#'
#' @param x A pscfit object with 2 treatment comparions
#' @export
#'
pscComb <- function(x) {
  drs <- as_draws(x$draws)
  direct <- drs$beta_2
  indirect <- drs$beta_2 - drs$beta_1

  rho <- cor(direct, indirect)

  sig_d <- sd(direct)^2
  sig_i <- sd(indirect)^2

  w <- sig_i / (sig_d + sig_i)

  cmb <- w*direct + (1-w)*indirect
  cmb

}
