# Check Value Ranges of Relevant Variables

Validates whether values in the dataset fall within expected ranges for
analysis. Used primarily as a safeguard to identify possible data entry
errors.

## Usage

``` r
check_ranges(d)
```

## Arguments

- d:

  A data frame prepared via the `prepare_data` function.

## Value

Prints a message listing any out-of-range values and may terminate
execution if invalid data are detected.

## See also

[`prepare_data()`](https://josefmana.github.io/demcrit/reference/prepare_data.md)
is a wrapper of this function.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- data_paths("data-raw")
data <- prepare_data(p)  # Automatically performs range checks
} # }
```
