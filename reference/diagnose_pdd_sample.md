# Diagnose All Patients

Applies all available diagnostic algorithms for probable Parkinson’s
disease dementia (PDD) across the full dataset. The set of criteria is
generated internally via the `specify_algorithms` function. Each patient
is assessed using each specification, and the corresponding diagnoses
and supporting information are recorded.

## Usage

``` r
diagnose_pdd_sample(d0)
```

## Arguments

- d0:

  A tibble containing the dataset prepared using `prepare_data`.

## Value

A list with two tibbles:

- `algorithms`:

  A tibble containing the full specification of all algorithms used.

- `diagnoses`:

  A tibble containing:

  - Patient ID

  - Criterion label (linked to the specification)

  - PDD diagnosis (`TRUE`/`FALSE`)

  - Individual diagnostic flags corresponding to Table 2 in Dubois et
    al. [(2007)](https://doi.org/10.1002/mds.21844)

  - Impairment labels across cognitive domains (per Level I criteria)

## References

Dubois, B., Burn, D., Goetz, C., Aarsland, D., Brown, R. G., Broe, G.
A., ... & Emre, M. (2007). Diagnostic procedures for Parkinson's disease
dementia: recommendations from the movement disorder society task force.
Movement disorders, 22(16), 2314-2324.
[doi:10.1002/mds.21844](https://doi.org/10.1002/mds.21844)

## See also

- [`prepare_data()`](https://josefmana.github.io/demcrit/reference/prepare_data.md)
  prepares `d0`.

- [`specify_algorithms()`](https://josefmana.github.io/demcrit/reference/specify_algorithms.md)
  runs internally to prepare a set of algorithms to be used.

- [`diagnose_pdd_case()`](https://josefmana.github.io/demcrit/reference/diagnose_pdd_case.md)
  runs internally to diagnose patients using a single algorithm.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- data_paths("data-raw")
data <- prepare_data(p)
pdd  <- diagnose_pdd_sample(data)
} # }
```
