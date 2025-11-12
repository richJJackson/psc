# Function for Plotting PSC objects

A function which visualises the data of a CFM or the combined CFM and DC
data for a 'psc' obecjec

## Usage

``` r
plotCFM(x, ...)
```

## Arguments

- x:

  an object of class 'CFM' or 'psc'

- ...:

  not used

## Value

a plot to describe the data included in the models

## Details

This function returns either density plots (continuous data) or waffle
plots (categroical data) to describe the data in the CFM. If an object
is supplied which has combied the CFM and DC (e.g. a psc object or an
object which has been passed through pscData()) then a comparison of the
CFM and DC will be supplied

## Examples

``` r
e4_data <- psc::e4_data
gemCFM <- psc::gemCFM
plotCFM(gemCFM)

psc <- pscfit(gemCFM,e4_data)
#> Warning: 54 rows removed due to missing data in dataset
plotCFM(psc)
```
