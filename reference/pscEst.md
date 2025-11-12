# Function for performing Bayesian MCMC estimation procedures in 'pscfit'

Function for performing Bayesian MCMC estimation procedures in 'pscfit'

## Usage

``` r
pscEst(pscOb, nsim = 1000, nchain = 1)
```

## Arguments

- pscOb:

  an pscOb object which has been passed through pscData() and init()
  functions

- nsim:

  the number of MCMC simulations to run

- nchain:

  Number of chains to use for analysis

## Value

A matrix containing the draws form the posterior distribution

## Details

Define the set of model parameters \\B\\ to contain \\\Gamma\\ which
summarize the parameters of the CFM. Prior distributions are defined for
B using a multivariate normal distribution \\\pi (B) \sim MVN(\mu
,\Sigma)\\ where \\\mu\|\\ is the vector of coefficient estimates from
the validated model and \\\Sigma\\ is the variance-covariance matrix.
This information is taken directly from the outputs of the parametric
model and no further elicitation is required. The prior distirbution for
the efficacy parameter (\\\pi{(\beta)}\\) is set as an uniformative
\\N(0,1000)\\.

Ultimately the aim is to estimate the posterior distribution for
\\\beta\\ conditional on the distribution of B and the observed data. A
full form for the posterior distribution is then given as

\$\$P(\beta \vert B,D) \propto L(D \vert B,\beta) \pi(B) \pi(\beta)\$\$

Please see 'pscfit' for more details on liklihood formation.

For each iteration of the MCMC procedure, the following algorithm is
performed

1.  Set and indicator s=1, and define an initial state based on prior
    hyperparameters for \\\pi(B)\\ and \\\pi(\beta)\\ such that \\b_s =
    \mu and \tau_s=0\\

2.  Update \\s = s+1\\ and draw model parameters \\b_s\\ from \\\pi(B)\\
    and an draw a proposal estimate of \\\beta\\ from some target
    distribution

3.  Estimate \\\Gamma\_(i,S)=\nu^T x_i\\ where \\\nu\\ is the subset of
    parameters from \\b_s\\ which relate to the model covariates and
    define 2 new likelihood functions \\\Theta\_(s,1)=L(D \vert
    B=b_s,\beta=\tau\_(s-1) )\\ & \\\Theta\_(s,2)= L(D \vert
    B=b_s,\beta=\tau_s)\\

4.  Draw a single value \\\psi\\ from a Uniform (0,1) distribution and
    estimate the condition \\\omega= \Theta\_(s,1)/\Theta\_(s,2)\\. If
    \\\omega \> \psi\\ then accept \\\tau_s\\ as belonging to the
    posterior distribution \\P(\beta \vert B,D)\\ otherwise retain
    \\\tau\_(s-1)\\

5.  Repeat steps 2 – 4 for the required number of iterations

The result of the algorithm is a posterior distribution for the log
hazard ratio, \\\beta\\, captures the variability in B through the
defined priors \\\pi{(\beta)}\\. @examples e4_data \<- psc::e4_data
gemCFM \<- psc::gemCFM pscOb \<- pscData(gemCFM,e4_data) pscOb \<-
init(pscOb) pscOb \<- pscEst(pscOb) importFrom survival Surv survfit
