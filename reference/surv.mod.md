# Example model for a survival outcome

A generated model with a survival endpoint and a cuymulative hazard
function estimated using flexible parametric splines. Data for the model
were synthetically generated and are based on a dataset to evaulate the
use of Sorafenib in HCC akin to the PROSASH model

## Usage

``` r
surv.mod
```

## Format

A model of class 'flezsurvreg':

- gamma:

  cumulative baseline hazard parameters

- vi:

  vascular invasion

- age60:

  patient age (centred at 60)

- ecog:

  ECOG performance Status

- logafp:

  AFP - log scale

- alb:

  albumin

- logcreat:

  Creatinine - log scale

- allmets:

  metastesis

- ageVasInv:

  centred age nested within vascular invasion

- time:

  survival time

- cen:

  censoring indicator

- os:

  survival time

- count:

  exapmple outcome for count data

- trt:

  exapmple identifier for mulitple treatment comparisons

- aet:

  Aetiology

## Source

simulated

## References

Using prognostic and predictive clinical features to make personalised
survival prediction in advanced hepatocellular carcinoma patients
undergoing sorafenib treatment. Berhane S, et al., Br J Cancer. 2019
Jul;121(2):117-124
