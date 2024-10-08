% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pscEst.flexsurvreg.R
\name{pscEst.flexsurvreg}
\alias{pscEst.flexsurvreg}
\title{Fucntion for estimating initial parameter values  'flexsurvreg'}
\usage{
\method{pscEst}{flexsurvreg}(CFM, DC_clean, nsim, start, trt = trt)
}
\arguments{
\item{CFM}{a model object supplied to pscfit}

\item{nsim}{the number of MCMC simulations to run}

\item{start}{the stating value for}

\item{trt}{an optional vector denoting treatment allocations where mulitple
treatment comparisons are bieng made}

\item{DC}{a dataset including covariates to match the CFM}
}
\value{
a matrix continig the draws form the posterior ditribution
}
\description{
Fucntion for estimating initial parameter values  'flexsurvreg'
}
\details{
An MCMC routine for fitting a psc model
}
