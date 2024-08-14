#' Function for performing estimation procedures in 'pscfit'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param nsim the number of MCMC simulations to run
#' @param start the stating value for
#' @param trt an optional vector denoting treatment allocations where mulitple
#'     treatment comparisons are bieng made
#' @details
#'
#' Define the set of model parameters \eqn{B} to contain \eqn{\Gamma} which summarize
#' the parameters of the CFM. Prior distributions are defined for B using a
#' multivariate normal distribution \eqn{\pi (B) ∼ MVN(\mu ,\Sigma)} where \eqn{\mu|}
#' is the vector of coefficient estimates from the validated model and \eqn{\Sigma}
#' is the variance-covariance matrix. This information is taken directly from the
#' outputs of the parametric model and no further elicitation is required.
#' The prior distirbution for the efficacy parameter (\eqn{\pi{(\beta)}}) is set
#' as an uniformative \eqn{~N(0,1000)}.
#'
#' Ultimately the aim is to estimate the posterior distribution for β conditional
#' on the distribution of B and the observed data.  A full form for the posterior
#' distribution is then given as
#'
#' \deqn{P(\beta|B,D) \propto L(D|B,\beta) \pi(B) π(\beta)}
#'
#' Please see 'pscfit' for more details on liklihood formation.
#'
#' For each iteration of the MCMC procedure, the following algorithm is performed \enumerate{
#'
#' \item{Set and indicator s=1, and define an initial state based on prior
#' hyperparameters for π(B)  and π(β) such that b_s = μ and τ_s=0}
#'
#' \item{Update s = s+1 and draw model parameters b_s from π(B) and an draw a
#' proposal estimate of β from some target distribution}
#'
#' \item{Estimate Γ_(i,S)=υ^T x_i where υ  is the subset of parameters from b_s
#'  which relate to the model covariates and define 2 new likelihood functions
#'    ϑ_(s,1)=L(D│B=b_s,β=τ_(s-1) ) & ϑ_(s,2)= L(D|B=b_s,β=τ_s)}
#'
#' \item{Draw a single value ς from a Uniform (0,1) distribution and estimate
#' the condition ω=  ϑ_(s,2)/ϑ_(s,2). If ω > ς then accept τ_s as belonging
#' to the posterior distribution P(β|B,D) otherwise retain τ_(s-1)}
#'
#' \item{Repeat steps 2 – 4 for the required number of iterations}
#'
#'}
#' The result of the algorithm is a posterior distribution for the log hazard ratio,
#' \eqn{\beta}, captures the variability in B through the defined priors \eqn{\pi{(\beta)}}.
pscEst <- function(x,...){
  UseMethod("pscEst")
}
