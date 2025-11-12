# Likelihood function for a psc model of class 'flexsurvreg'

A function which defines the likelihood for a PSC model where the
Counter Factual Model (CFM) takes the form of a 'flexsurvreg' object and
an efficacy parameter (\\\beta\\) is being estimated. For more details
on fitting please see ?pscfit and ?pscEst

## Usage

``` r
lik.flexsurvreg(beta, pscOb)
```

## Arguments

- beta:

  a parameter to be estimate

- pscOb:

  A pscOb object containing a cleaned dataset including covariates to
  match the CFM

## Value

the results of a likelihood functions

## Details

A likelihood function for use by pscfit for a model of class
'flexsurvreg'
