# Updating the posterior distribution as part of the MCMC estimation process A procedure which performs a single update of the posterior distribution

Updating the posterior distribution as part of the MCMC estimation
process A procedure which performs a single update of the posterior
distribution

## Usage

``` r
pscEst_update(i, draws, pscOb)
```

## Arguments

- i:

  index of the draw number (i\>1)

- draws:

  a matrix containing the posterior draws to update

- pscOb:

  an pscOb object which has been passed through pscData() and init()
  functions

## Value

An updated set of posterior draws
