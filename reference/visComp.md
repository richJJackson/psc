# Visualising Comparisons between a CFM and a DC

The visComp function takes the data visualisations supplied as part of
the CFM model and appends summaries of the equivalent datapoints from
the Data Cohort.

## Usage

``` r
visComp(CFM, DC, id = NULL)
```

## Arguments

- CFM:

  an object of class pscCFM

- DC:

  A dataset including columns to match to covariates in the model

- id:

  Numeric vector stating which patient(s) from the dataset should be
  included in the analysis. Defaults to all patients

## Value

a list of grobs for each model covariate
