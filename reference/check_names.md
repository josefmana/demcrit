# Check Patient Names

Compares patient names between a manually entered dataset and a
reference set of correct names (e.g., from REDCap) to identify
discrepancies.

## Usage

``` r
check_names(d, nms)
```

## Arguments

- d:

  A data frame containing patient names to be checked (e.g.,
  hand-written data).

- nms:

  A data frame or vector containing the correct patient names (e.g.,
  from REDCap).

## Value

Prints a message listing any discrepancies found. Intended to be called
within the
[`prepare_data()`](https://josefmana.github.io/demcrit/reference/prepare_data.md)
function, where it can optionally be silenced.

## See also

- [`prepare_data()`](https://josefmana.github.io/demcrit/reference/prepare_data.md)
  is a wrapper of this function.

- [`import_item_data()`](https://josefmana.github.io/demcrit/reference/import_item_data.md)
  prepares `d`.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- data_paths("data-raw")

# With name checking:
data <- prepare_data(p, check.names = TRUE)

# Without name checking:
data <- prepare_data(p, check.names = FALSE)
} # }
```
