# Generate a Summary Table of Algorithms

This function creates a pre-specified summary table of the probable PDD
algorithms compared in the study. It is intended as a shortcut to insert
a clean table into a Quarto document without cluttering the main text.
The table content is hard-coded and can only be changed by modifying the
function code.

## Usage

``` r
table_algorithms(vars)
```

## Arguments

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

## Value

A `gt` table object containing the summary of criteria used in the
study.

## See also

[`gt_apa_table()`](https://josefmana.github.io/demcrit/reference/gt_apa_table.md)
is used to format the table.
