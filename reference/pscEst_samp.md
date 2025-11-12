# Starting conditions for Bayesian MCMC estimation procedures in 'pscfit' A procedure which runs the sampling process for MCMC estimation

Starting conditions for Bayesian MCMC estimation procedures in 'pscfit'
A procedure which runs the sampling process for MCMC estimation

## Usage

``` r
pscEst_samp(pscOb, nsim)
```

## Arguments

- pscOb:

  an pscOb object which has been passed through pscData() and init()
  functions

- nsim:

  the number of MCMC simulations to run

## Value

An updated set of attributes for the pscOb which includes
