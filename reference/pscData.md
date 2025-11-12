# A function which structures the Data Cohort in a format for model estimation

This function ensures the data are supplied in a structure which allows
for estimation. This is performed by re-fitting the original CFM with
the DC and extracting the appropriate structures. Data are returned in
terms of "Y" for model outcomes, "X" for data and "Z" for random effects
where mixed models are supplied.

## Usage

``` r
pscData(CFM, DC, id = NULL, trt = NULL)
```

## Arguments

- CFM:

  a Counter Factual Model

- DC:

  a Data Cohort object

- id:

  to be specified for subgroup analysis. Defaults to NULL

- trt:

  to be specified for multiple treatment comparisons. Defaults to NULL

## Value

A set of structures for use with estimation procedures

## Examples

``` r
e4_data <- psc::e4_data
gemCFM <- psc::gemCFM
pscOb <- pscData(gemCFM,e4_data)
#> Warning: 54 rows removed due to missing data in dataset
```
