% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pscEst.glm.R
\name{pscEst.glm}
\alias{pscEst.glm}
\title{Fucntion for estimating initial parameter values  'glm'}
\usage{
\method{pscEst}{glm}(CFM, DC_clean, nsim, start)
}
\arguments{
\item{CFM}{a model object supplied to pscfit}

\item{nsim}{the number of MCMC simulations to run}

\item{start}{the stating value for}

\item{DC}{a dataset including covariates to match the CFM}
}
\value{
a matrix continig the draws form the posterior ditribution
}
\description{
Fucntion for estimating initial parameter values  'glm'
}
\details{
An MCMC routine for fitting a psc model
}
