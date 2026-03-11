# Prepare Defaults for Data Simulation

Prepares a list with default values for data simulation. These defaults
are derived from the data used as a basis of the demcrit article. This
function is a helper with no inputs and is typically called within
`simulate_PDD_data`.

## Usage

``` r
prepare_defaults()
```

## Value

A list containing:

- `Mu`:

  Named numeric vector containing means for FAQ, MMSE, MoCA, and sMoCA.

- `Sigma`:

  Named matrix containing variance-covariance of the variables (FAQ,
  MMSE, MoCA, and sMoCA).

- `cens`:

  A matrix with named rows denoting scales (FAQ, MMSE, MoCA, and sMoCA)
  and two columns - the first containing lower bound, the second
  containing upper bound of each scale.

- `crits`:

  A data.frame with four columns denoting IADL measure to use (`IADL`),
  threshold for the IADL measure (`IADL_thres`), cognitive measure to
  use (`cognition`), and threshold for the cognitive measure
  (`cognition_thres`). Score above `IADL_thres` and below
  `cognition_thres` are regarder to indicate PDD symptoms. Rownames
  contain algorithm names.

## See also

[`simulate_pdd_data()`](https://josefmana.github.io/demcrit/reference/simulate_pdd_data.md)
uses outputs of this function unless specified by the user.
