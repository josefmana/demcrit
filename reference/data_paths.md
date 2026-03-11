# Define Raw Data Paths

Prepares a set of file paths pointing to all required raw data files
used in the analysis. The function enforces naming conventions and
validates compatibility between the contents of the specified directory
and the expected structure. It is intended to be used as input for the
`prepare_data` function.

## Usage

``` r
data_paths(dir = "data-raw")
```

## Arguments

- dir:

  A character string specifying the input directory where raw data files
  reside.

## Value

A named character vector with file paths to all required raw data files.

## See also

[`prepare_data()`](https://josefmana.github.io/demcrit/reference/prepare_data.md)
is the next step in analysis pipeline.

## Examples

``` r
if (FALSE) { # \dontrun{
p    <- data_paths("data-raw")
data <- prepare_data(p)
} # }
```
