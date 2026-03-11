# Specify Algorithms for Probable PDD

Computes a Cartesian product of possible diagnostic components to define
all combinations of algorithms for probable Parkinson’s disease dementia
(PDD). This function is a helper with no inputs and is typically called
within `diagnose_pdd_sample`.

## Usage

``` r
specify_algorithms()
```

## Value

A tibble where each row represents one unique diagnostic algorithm for
probable PDD. Columns indicate the type of algorithms, individual test
indexes, thresholds, and other relevant parameters.

## See also

[`diagnose_pdd_sample()`](https://josefmana.github.io/demcrit/reference/diagnose_pdd_sample.md)
combines the algorithms with data to diagnose single patients.
