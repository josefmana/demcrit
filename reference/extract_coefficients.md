# Extract point estimate of model coefficients

Given posterior draws from a brms model or projpred projection and
scaling values for predictors, converts coefficients from scaled
predictors to the original variable scale and returns summary
statistics.

## Usage

``` r
extract_coefficients(mod, scls, stat = "mean", ...)
```

## Arguments

- mod:

  Model object or posterior draws compatible with
  [`posterior::as_draws_matrix()`](https://mc-stan.org/posterior/reference/draws_matrix.html).

- scls:

  Scaling values as exported by `fit_reference`.

- stat:

  Summary statistic applied to posterior draws.

- ...:

  Unused. There to allow for compatibility with
  `run_scoring_rule_pipeline`.

## Value

Named numeric vector of coefficients on the original scale.
