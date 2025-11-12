# A function which performs error checks between the DC and CFM

The purpose of this function is check that terms included in the Data
Cohort match those used within the Counter Factual Model. This acts as a
sub-function to the pscData.R function.

## Usage

``` r
pscData_error(term.nm, DC)
```

## Arguments

- term.nm:

  Term names from the CFM

- DC:

  a data cohort to be 'cleaned'

## Value

a 'stop' command when errors are detected
