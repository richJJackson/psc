# Counter Factual Model - summary

A function to estimate the linear predictor - used in bootstrapping CFM
for CIs

## Usage

``` r
boot_lp(i, pscOb, resp = resp, rest = NULL)
```

## Arguments

- i:

  indicator object

- pscOb:

  an object of class 'psc'

- resp:

  A boolean object to determine if results should be presented on the
  response scale

- rest:

  A matrix of sample covariate estimates

## Value

A simulated set of responses
