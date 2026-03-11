# Generate a Summary Table of Estimands

This function creates a pre-specified table that provides a verbal
summary of the study's estimands for inclusion in the Appendix. It is
intended as a shortcut to insert a clean, structured description into a
Quarto document without cluttering the main text. The table content is
fixed and can only be modified by editing the function's code directly.

## Usage

``` r
table_estimands()
```

## Value

A `gt` table object summarising the theoretical estimands, empirical
estimands, and the corresponding statistical estimates used in the
study.

## See also

[`gt_apa_table()`](https://josefmana.github.io/demcrit/reference/gt_apa_table.md)
is used to format the table.
