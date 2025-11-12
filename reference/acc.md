# acc

Function to accept (or not) a proposed solution used as part of the MCMC
procedure

## Usage

``` r
acc(old, new)
```

## Arguments

- old:

  a numeric value

- new:

  a numeric value

## Value

returns the an evaluation of old/new \> U where U is a draw from the
uniform distribution

## Details

A function for the evaluation of two likelihoods as part of the MCMC
procedure
