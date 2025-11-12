# Example Dataset of patients with aHCC receiving Lenvetanib

A dataset containing 100 simulated patients. Data are based on the data
used to generate PROSASH survival model -see ?psc::surv.mod for more
detials.

## Usage

``` r
data
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
