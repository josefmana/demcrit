# Diagnose via a Single Algorithm

Applies a specified set of diagnostic criteria to a data matrix
representing patient-level data to determine whether each patient meets
the criteria for probable Parkinson’s disease dementia (PDD). This
function is intended to be used internally by the wrapper function
`diagnose_pdd_sample`.

## Usage

``` r
diagnose_pdd_case(
  x,
  c,
  p = c("moca_7", "moca_cloc", "vf_k", "moca_5words", "moca_cube", "moca_abs",
    "moca_anim", "mmse_7", "cloc", "vf_s", "mmse_3words", "mmse_pent")
)
```

## Arguments

- x:

  A data frame or matrix containing patient-level variables used for
  diagnosis.

- c:

  A named vector (or single-row data frame) specifying the algorithms to
  be applied for PDD diagnosis.

- p:

  A character vector containing variable names to be used later during
  projective prediction feature selection. Defaults to all Level I
  screenings subtask defined in the algorithm set.

## Value

A logical vector indicating probable PDD diagnosis for each patient
(`TRUE` = diagnosed, `FALSE` = not diagnosed).

## See also

[`diagnose_pdd_sample()`](https://josefmana.github.io/demcrit/reference/diagnose_pdd_sample.md)
wraps the function to diagnose all patients.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- data_paths("data-raw")
data <- prepare_data(p)
crit <- specify_algorithms()
pdd  <- diagnose_pdd_case(data, crit[1, ])
} # }
```
