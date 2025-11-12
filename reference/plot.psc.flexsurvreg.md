# Function for Plotting PSC objects

Function for Plotting PSC objects

## Usage

``` r
# S3 method for class 'psc.flexsurvreg'
plot(pscOb, addFit = T, ...)
```

## Arguments

- pscOb:

  an object of class 'psc'

- addFit:

  should a curve for the model fit be added?

- ...:

  not used

## Value

a survival plot corresponding to the psc fit

## Details

making use of 'ggsurvplot' in the survminer package, this function plots
the expected survival funtion for the 'control' treatment estimated from
the CFM along with the Kaplan Meier estimates of the observed events
