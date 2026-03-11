# Derive a scoring rule for PDD

Given a prediction linear logistic model and its ROC analysis results,
computes a scoring rule for deciding a patient suffers probable PDD
given screening criteria.

## Usage

``` r
derive_scoring_rule(pt, coefs, inverse = TRUE, ...)
```

## Arguments

- pt:

  The threshold on probability scale. E.g., derived from `run_roc`.

- coefs:

  Model coefficients as computed by
  [`extract_coefficients()`](https://josefmana.github.io/demcrit/reference/extract_coefficients.md).

- inverse:

  Should `pt` be inversed from probability to logit scale?

- ...:

  Unused. There to allow for compatibility with
  `run_scoring_rule_pipeline.`

## Value

List containing:

- score coefficients

- cutoff

- equation (character)

- integer scoring rule
