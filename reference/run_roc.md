# Run ROC analysis

Given observed values and predictions, runs ROC analysis.

## Usage

``` r
run_roc(y_obs, preds, prevs = c(0.1, 0.3, 0.5), ...)
```

## Arguments

- y_obs:

  Observed values

- preds:

  Predictions. Does not need to be on the same scale as observed values.
  Can be single test scores or expected probabilities from a logistic
  regression or any other model.

- prevs:

  Prevalences.

- ...:

  Unused. There to allow for compatibility with
  `run_scoring_rule_pipeline`.

## Value

List with two components:

- ROC:

  List of class "roc", including AUC plot.

- thresholds:

  Tibble with the best thresholds according to assumed PDD prevalences
  with additional columns for specificity, sensitivity, and accuracy in
  current sample.
