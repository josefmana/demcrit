# Run pipeline deriving scoring rule for PDD

Using different assumed PDD prevalences and different models, computes a
set of ROC analyses and derives decision rules for the best threshold
identified by each.

## Usage

``` r
run_scoring_rule_pipeline(
  y_obs,
  model,
  scaling,
  linear = TRUE,
  ...,
  nms = NULL
)
```

## Arguments

- y_obs:

  Observed outcome values.

- model:

  Model object used for prediction (needs to be Bayesian and ammenable
  to functions from the {posterior}package.

- scaling:

  A scaling table with columns "x" for predictor name, "M" for mean and
  "SD" for standard deviation..

- linear:

  Should predictions from the linear model (`TRUE`) or on the response
  scale (`FALSE`) be used?

- ...:

  Other parameters going either to `run_roc` if `prevs`, to
  `extract_coefficients` if `stat`, to or to `compute_predictions` if
  `seed`.

- nms:

  Optional coefficient names, including intercepts. Double check they
  are in correct order!

## Value

description
