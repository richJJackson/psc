# A function which structures the Data Cohort in a format for model estimation

This function ensures the data are supplied in a structure which allows
for estimation. This is performed by re-fitting the original CFM with
the DC and extracting the appropriate structures. Data are returned in
terms of "Y" for model outcomes, "X" for data and "Z" for random effects
where mixed models are supplied.

## Usage

``` r
pscData_structure(CFM, DC)
```

## Arguments

- CFM:

  a Counter Factual Model

- DC:

  a Data Cohort object

## Value

A set of structures for use with estimation procedures re-export Surv
from survival
