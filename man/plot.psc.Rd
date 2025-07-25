% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.psc}
\alias{plot.psc}
\title{Function for Plotting PSC objects}
\usage{
\method{plot}{psc}(x, ...)
}
\arguments{
\item{x}{an object of class 'psc'}

\item{...}{not used}
}
\value{
a survival plot corresponding to the psc fit
}
\description{
A function which illsutrates the predicted response under the Counter Factual
Model (CFM) and the observed response under the experimental treatment(s).
Form of the output will depend on the form of the CFM used
}
\details{
This function plots the expected response of the control treatment
   along with the observe response rates of the experimental arms
}
\examples{
library(psc)
e4_data <-psc::e4_data
gemCFM <- psc::gemCFM
psc <- pscfit(gemCFM,e4_data)
plot(psc)
}
