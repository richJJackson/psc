# Function for Plotting PSC objects

A function which illsutrates the predicted response under the Counter
Factual Model (CFM) and the observed response under the experimental
treatment(s). Form of the output will depend on the form of the CFM used

## Usage

``` r
# S3 method for class 'psc'
plot(x, ...)
```

## Arguments

- x:

  an object of class 'psc'

- ...:

  not used

## Value

a survival plot corresponding to the psc fit

## Details

This function plots the expected response of the control treatment along
with the observe response rates of the experimental arms

## Examples

``` r
e4_data <- psc::e4_data
gemCFM <- psc::gemCFM
psc <- pscfit(gemCFM,e4_data)
#> Warning: 54 rows removed due to missing data in dataset
plot(psc)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the ggpubr package.
#>   Please report the issue at <https://github.com/kassambara/ggpubr/issues>.
#> Ignoring unknown labels:
#> • colour : "Strata"
```
