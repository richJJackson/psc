# Counter Factual Model - summary

A function to estimate the survival function based on parameter
estimates - used in ootstrapping CFM for CIs

## Usage

``` r
boot_sest(
  i,
  pscOb = pscOb,
  lam = lam,
  kn = kn,
  k = k,
  cov = cov,
  tm = tm,
  rest = rest,
  beta = beta
)
```

## Arguments

- i:

  indicator object

- pscOb:

  a pscOb object

- lam:

  parameters of the flexible spline model

- kn:

  knots included in the flexible spline model

- k:

  number of knots in the flexible spline model

- cov:

  a matrix of covariates

- tm:

  time at which to assess the survival function

- rest:

  a set of parameter covariate draws

- beta:

  parameter with which to adjust the baseline function

## Value

A set of survival estimates
