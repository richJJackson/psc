# Running the Bayesian MCMC routine A procedure which runs the MCMC estimation routine

Running the Bayesian MCMC routine A procedure which runs the MCMC
estimation routine

## Usage

``` r
pscEst_run(pscOb, nsim, nchain)
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

## Examples

``` r
e4_data <- psc::e4_data
gemCFM <- psc::gemCFM
pscOb <- pscData(gemCFM,e4_data)
#> Warning: 54 rows removed due to missing data in dataset
pscOb <- init(pscOb)
pscOb <- pscEst_start(pscOb,nsim=1000,nchain=2)
pscOb <- pscEst_run(pscOb,nsim=1000,nchain=2)
```
