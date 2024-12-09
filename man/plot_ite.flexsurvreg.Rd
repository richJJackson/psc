% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ite.flexsurvreg.R
\name{plot_ite.flexsurvreg}
\alias{plot_ite.flexsurvreg}
\title{Function for Plotting PSC objects}
\usage{
plot_ite.flexsurvreg(x)
}
\arguments{
\item{x}{an object of class 'psc'}
}
\value{
A plot showing the individual treatment effects
}
\description{
A function which plots a patients response against their predicted response
from a CFM
}
\details{
This function plots the expected response of the control treatment
   along with the observe response rates for each patient in the dataset
}
\examples{
library(psc)
library(survival)
surv.mod <- psc::surv.mod
data <- psc::data
surv.psc <- pscfit(surv.mod,data)
plot_ite(surv.psc)
}
