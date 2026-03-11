# Generate an APA-style Table

Converts a tibble or data frame into an APA-formatted table using the gt
package. The resulting object inherits all the features and
customizations provided by `gt`.

## Usage

``` r
gt_apa_table(x, grp = NULL, nms = NULL, tit = "")
```

## Arguments

- x:

  A tibble or data frame to be formatted as an APA-style table.

- grp:

  An optional column name (as a string or symbol) to use for grouping
  rows via [`gt::gt()`](https://gt.rstudio.com/reference/gt.html)'s
  `groupname_col` parameter. Defaults to `NULL`.

- nms:

  An optional column name (as a string or symbol) to use for row names
  via [`gt::gt()`](https://gt.rstudio.com/reference/gt.html)'s
  `rowname_col` parameter. Defaults to `NULL`.

- tit:

  A character string specifying the table title. Will be converted to
  HTML format. Defaults to `""`.

## Value

A `gt` table object formatted according to APA style.
