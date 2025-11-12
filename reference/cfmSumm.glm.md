# Counter Factual Model - summary

A generic function to provide a summary of a Counter factual model of
class 'glm'

## Usage

``` r
cfmSumm.glm(pscOb, bootCI = TRUE, nboot = 1000, resp = TRUE)
```

## Arguments

- pscOb:

  an object of class 'psc'

- bootCI:

  a boolean to determine if bootstrapping CIs are required

- nboot:

  Number of bootstraps

- resp:

  Should results be on the response scale?

## Value

A summary of a cfm object
