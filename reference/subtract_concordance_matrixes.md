# Compare concordance matrixes

Given two sets of `describe_concordance` results, computes difference
matrixes for Cohen's Kappa, Accuracy, Sensitivity, and Specificity.

## Usage

``` r
subtract_concordance_matrixes(conc0, conc1, label0 = "0", label1 = "1")
```

## Arguments

- conc0:

  The subtrahend concordance statistics

- conc1:

  The minuend concordance statistics

- label0:

  How should the subtrahend be called?

- label1:

  How should the minuend be called?

## Value

A list with two components:

- `differences`:

  Tibble containing rowwise differences

- `plots`:

  A list of ggplot2-based visualisations of differences
