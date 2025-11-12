# Counter Factual Model - summary

A function to estimate the survival function based on parameter
estimates - used in ootstrapping CFM for CIs

## Usage

``` r
spline_surv_est(lam, kn, k, haz_co, cov_co, cov = cov, tm = tm, beta = 0)
```

## Arguments

- lam:

  parameters of the flexible spline model

- kn:

  knots included in the flexible spline model

- k:

  number of knots in the flexible spline model

- haz_co:

  parameters for the baseline hazard function in the flexible spline
  model

- cov_co:

  covariate parameters of the flexible spline model

- cov:

  a matrix of covaraites from the Data Cohort

- tm:

  time at which to assess the survival function

- beta:

  parameter with which to adjust the baseline function (defaults to
  beta=0)

## Value

A data frame containing survival estimates for a give time
