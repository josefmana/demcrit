# Order and Prints Algorithms

This function computes summaries of the estimated PDD rates, stratified
by diagnostic algorithm.

## Usage

``` r
summarise_rates(d0, vars, descending = TRUE, plot = TRUE)
```

## Arguments

- d0:

  A list with PDD data generated via `diagnose_pdd_sample`.

- vars:

  A data.frame, tibble, or matrix with in the following order:

  1.  variable names,

  2.  variable labels

  3.  type of variable (continuous, binary, or nominal)

  4.  optional, group,

  5.  optional, mapping each label to its description in the table’s
      note.

  Alternatively, a path to a CSV file (semicolon-delimited) containing
  such a table.

- descending:

  A logical indicating whether algorithms with the highest estimated PDD
  rates should be listed first (default is `TRUE`).

- plot:

  A logical indicating whether to plot PDD rates as densities. The code
  is dataset-specific (default is `TRUE`).

## Value

A list with:

- `table`:

  A tibble containing raw summaries of PDD rates.

- `plot`:

  A `ggplot2` object visualising estimated PDD rates.

- `gtable`:

  A named list of gt tables:

  `gtab_rates`

  :   An APA-style `gt` table summarising the results.

  `gtab_algos`

  :   An APA-style `gt` table listing algorithms used.

## See also

[`diagnose_pdd_sample()`](https://josefmana.github.io/demcrit/reference/diagnose_pdd_sample.md)
prepares `d0`.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- data_paths("data-raw")
data <- prepare_data(p)
pdd  <- diagnose_pdd_sample(data)
vars <- here::here("data-raw", "VariablesOfInterest.csv")

rates0 <- summarise_rates(pdd, vars)
rates1 <- summarise_rates(pdd, vars, TRUE)  # rates0 and rates1 are identical
rates2 <- summarise_rates(pdd, vars, FALSE) # ascending order
} # }
```
