# Starting conditions for Bayesian MCMC estimation procedures in 'pscfit' A procedure which sets the starting conditions for MCMC estimation

Starting conditions for Bayesian MCMC estimation procedures in 'pscfit'
A procedure which sets the starting conditions for MCMC estimation

## Usage

``` r
pscEst_start(pscOb, nsim, nchain)
```

## Arguments

- pscOb:

  an pscOb object which has been passed through pscData() and init()
  functions

- nsim:

  the number of MCMC simulations to run

- nchain:

  Number of chains to use for analysis

## Value

An updated set of attributes for the pscOb which includes

## Details

A procedure which sets the starting conditions for MCMC estimation
including defining starting estimates, setting a matrix for draws to be
save in and defining, target and prior distributions and deifnign the
posterior desitribution from the CFM. This also sets the number of cores
to be used for estimation where parallel computing is applied.

## Examples

``` r
e4_data <- psc::e4_data
gemCFM <- psc::gemCFM
pscOb <- pscData(gemCFM,e4_data)
#> Warning: 54 rows removed due to missing data in dataset
pscOb <- init(pscOb)
pscOb <- pscEst_start(pscOb,nsim=1000,nchain=2)
```
