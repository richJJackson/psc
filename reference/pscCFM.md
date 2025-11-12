# Creating a CFM model which can be shared

Standard R model objects contain within them the datasets used to create
the model and as such care is needed when sharing these objects for
research. The psc.cfm function creates an object with all identifiable
information retracted and includes only the information required to use
the models within the psc package

## Usage

``` r
pscCFM(CFM, dataSumm = T, dataVis = T)
```

## Arguments

- CFM:

  a 'glm' or 'flexsurvreg' model object

- dataSumm:

  a logical indicator specifying whether a summary of the data should be
  provided, defaults to TRUE.

- dataVis:

  a logical indicator specifying whether a visualisations of the data
  should be provided, defaults to TRUE.

## Value

a list containing objects which specify the required exported components
of the model.
