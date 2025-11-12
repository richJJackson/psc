# Model for a survival outcome based on Gemcitbine patients from ESPAC-3

A generated model with a survival endpoint and a cuymulative hazard
function estimated using flexible parametric splines. Data for the model
were obtained from the ESPAC-3 trials

## Usage

``` r
gemCFM
```

## Format

A model of class 'pscCFM' containg a 'flexsurvreg' model:

- gamma:

  cumulative baseline hazard parameters

- nodes:

  negative (n=1) or positive (n=2) lymph nodes

- grade:

  tumour grade (1,2 or 3)

- lca199:

  log transformed ca19.9

- t:

  T-stage (1,2 or 3)

## Source

simulated
