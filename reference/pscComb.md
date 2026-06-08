# PSC Estiamtion combination

This function allows the combination of estimation where mulitple
treatment comparisons are being made and one of the treatment levels
relates to the control level. Note here it is assumed that the control
level is coded trt=0. This function then combines the two efficacy
estimates for the treatment effect (concurrent and synhtetic) whilst
accounting for the correlation inherent in these two estimates.

## Usage

``` r
pscComb(x)
```

## Arguments

- x:

  A pscfit object with 2 treatment comparions
