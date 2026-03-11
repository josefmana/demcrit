# Fit Reference Model for Projective Prediction

Using a regularised horseshoe prior, fits a full reference model of
selected "gold standard" PDD diagnosis using specified predictor set.

## Usage

``` r
fit_reference(
  d,
  gs = "Lvl.II (1)",
  y = "PDD",
  X = c("moca_7", "moca_cloc", "vf_k", "moca_5words", "moca_cube", "moca_abs",
    "moca_anim", "mmse_7", "cloc", "vf_s", "mmse_3words", "mmse_pent"),
  expect_nonzero = 5,
  ...
)
```

## Arguments

- d:

  Data such as those calculated by `diagnose_pdd_sample`.

- gs:

  Name the "gold standard" variable has in `d$type`.

- y:

  Name of the PDD diagnosis variable in `d`

- X:

  Character vector containing all predictor variables from `d`.

- expect_nonzero:

  Number of variables expected to be truly predictive.

- ...:

  Other parameters for
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

## Value

List with three components:

- model:

  brmsfit with the fitted model

- data:

  Tibble with raw input data

- scaling:

  Tibble with predictors' mean and SDs

## Details

...

## See also

- [`brms::horseshoe()`](https://paulbuerkner.com/brms/reference/horseshoe.html)
  explains the prior implementation.
