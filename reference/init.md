# Function for estimating initial parameter values

Function for estimating initial parameter values

## Usage

``` r
init(pscOb)
```

## Arguments

- pscOb:

  a psc object

## Value

Parameter Estimates and standard error for the efficacy parameter

## Details

This function takes the likelihood and data structures provided by the
pscData() strucutres and fits the likelihood to provide starting values
for MCMC estimation
