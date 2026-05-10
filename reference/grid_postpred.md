# Grid posterior predictions of a model

Given a grid of predictor values, their scaling, and a Bayesian logistic
regression model, prints a prediction grid heatmap.

## Usage

``` r
grid_postpred(
  mod,
  pgrid = NULL,
  scls = NULL,
  perc = 0.95,
  lt = c(-1.29335, -2.649892),
  cols = c("grey80", "black", "red4"),
  pal = "viridis",
  dir = 1
)
```

## Arguments

- mod:

  brms or projpred model

- pgrid:

  A grid of predictor values.

- scls:

  A scaling table with columns "x" for predictor name, "M" for mean and
  "SD" for standard deviation.

- perc:

  Percentage value for computation of the equal-tailed posterior
  interval \\ETI\\.

- lt:

  Logit threshold(s) for declaring PDD.

- cols:

  Text colours for separating PDD classification according to thresholds
  in `lt`. Must be of length `length(lt) + 1`

- pal:

  Colour pallete pushed to
  [`ggplot2::scale_fill_viridis_c()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html).

- dir:

  Which way should be `pal` scaled? Pushed to
  [`ggplot2::scale_fill_viridis_c()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html)
  as `direction`.

## Value

{ggplot2} heatmap with estimates and `perc`% ETIs.
