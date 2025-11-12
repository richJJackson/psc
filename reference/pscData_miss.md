# A function which removes missing data from the DC

Currently the psc package works only on complete-case datasets. This
function removes rows with missing data and returns a warning to inform
the user. This acts as a sub-function to the pscData.R function.

## Usage

``` r
pscData_miss(DC)
```

## Arguments

- DC:

  a data cohort to be 'cleaned'

## Value

a dataset with missing data removed
