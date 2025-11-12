# A function to ensure that data from the cfm and data cohort are compatible

The purpose of this function is to run a series of checks to ensure that
the data included in the data cohort is comparable to the
counter-factual model. This matches the data classes and checks the
levels in the DC match those used in the CFM. This acts as a
sub-function to the pscData.R function.

## Usage

``` r
pscData_match(cls, lev, DC)
```

## Arguments

- cls:

  a list of extracted data classes

- lev:

  a list of factor levels

- DC:

  a data cohort to be 'cleaned'

## Value

a dataset which is checked and compatible with the CFM
