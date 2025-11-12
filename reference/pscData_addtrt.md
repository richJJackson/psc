# A function that includes a treatment indicator when multiple treatment comparisons are required

The purpose of this function is to organise a treatment indicator where
multiple treatment comparisons are being evaluated. This acts as a
sub-function to the pscData.R function.

## Usage

``` r
pscData_addtrt(DC, trt)
```

## Arguments

- DC:

  a data cohort to be 'cleaned'

- trt:

  a treatment indicator

## Value

a dataset which is checked and compatible with the CFM
