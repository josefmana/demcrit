# Compute model predictions

Given a Bayesian "projpred" or "brms" model, computes linear and full
posterior predicitons.

## Usage

``` r
compute_predictions(model, seed = NULL, ...)
```

## Arguments

- model:

  The model

- seed:

  Optional seed for reproducibility

- ...:

  Unused. There to allow for compatibility with
  `run_scoring_rule_pipeline`.

## Value

Tibble with two columns:

- epred:

  Linear prediction on the logit scale

- ppred:

  Posterior prediction on the outcome scale
