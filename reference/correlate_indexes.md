# Compute correlation between test indexes

Given raw data, computes selected type of pairwise correlations
("Pearson" by default) of all selected indexes and plots them.
Optionally, the user can specify an "ideal" correlation matrix of the
same variables for comparison purposes.

## Usage

``` r
correlate_indexes(
  d0,
  v = setNames(c("moca_7", "mmse_7", "moca_cloc", "cloc", "vf_s", "vf_k", "moca_5words",
    "mmse_3words", "moca_cube", "mmse_pent", "moca_abs", "moca_anim"), c("MoCA Sevens",
    "MMSE Sevens", "MoCA Clock", "Clock Drawing", "VF S", "VF K", "MoCA 5 words",
    "MMSE 3 words", "MoCA Cube", "MMSE Pent.", "MoCA Abs.", "MoCA An.")),
  ideal = NULL,
  ...
)
```

## Arguments

- d0:

  Input data.

- v:

  Named character vector of variables to be included.

- ideal:

  Optionally a matrix of ideal values. Write `NULL` for the default or
  `NA` if no ideal matrix should be plotted.

- ...:

  Other variables for [`cor()`](https://rdrr.io/r/stats/cor.html)

## Value

A list containing:

- R_observed:

  Correlation matrix of observed data

- R_ideal:

  Correlation matrix of the "ideal" pattern
