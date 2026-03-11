# Summarise Data

Computes descriptive summaries of a given vector, optionally including
graphical representation (planned for future versions). Supports various
summary types tailored for continuous, binary, and nominal variables.

## Usage

``` r
do_summary(x, dec, sum = NULL)
```

## Arguments

- x:

  A vector of data to be summarised.

- dec:

  An integer specifying the number of decimal places for numeric
  summaries.

- sum:

  A character string specifying the type of summary to compute. Possible
  values:

  `NULL` (default)

  :   Simple printing of the vector

  "N"

  :   Number of observations

  "msd"

  :   Mean +/- standard deviation

  "M"

  :   Mean

  "SD"

  :   Standard deviation

  "Md"

  :   Median

  "IQR"

  :   Interquartile range

  "minmax"

  :   Minimum-maximum range

  "Min"

  :   Minimum

  "Max"

  :   Maximum

  "p"

  :   p-value (formatted for tables)

  "ptext"

  :   p-value (formatted for in-text reporting)

  "Nperc"

  :   Count and percentage for binary variables

  "Nslash"

  :   Counts separated by slashes for nominal variables with any number
      of categories

  "estCI"

  :   Estimate with confidence interval, i.e., `estimate [CI]`

## Value

A character string or numeric summary as specified by `sum`.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- data_paths("data-raw")
data <- prepare_data(p)

M <- do_summary(data[, 25], 2, "M")
SD <- do_summary(data[, 25], 2, "SD")
Md <- do_summary(data[, 25], 2, "Md")
mm <- do_summary(data[, 25], 0, "minmax")
} # }
```
