# Personalised Synthetic Controls - summary

A generic function to provide a summary of a 'psc' object obtained from
pscfit.R

## Usage

``` r
# S3 method for class 'psc'
summary(object, ...)
```

## Arguments

- object:

  an object of class 'psc'

- ...:

  not used

## Value

A summary of a psc object obtained using pscSumm and a copy of the
pscfit object

## Examples

``` r
e4_data <- psc::e4_data
gemCFM <- psc::gemCFM
psc <- pscfit(gemCFM,e4_data)
#> Warning: 54 rows removed due to missing data in dataset
summary(psc)
```
