# Visualising data within a CFM

The pscCFM creates a model object which is stripped of identifiable
information. The cfmDataVis function supplies a visualised form of the
dataset for summary information

## Usage

``` r
cfmDataVis(cfm)
```

## Arguments

- cfm:

  a 'glm' or 'flexsurvreg' model object

## Value

a list of grobs for each model covariate
