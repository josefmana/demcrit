# Validation of Algorithms for PDD

This package is **not** intended for broad public use. Its main purpose is to:

1. provide transparent documentation of the data analyses forming the backbone
of the empirical part of *Mana et al. (under review)*, and

2. supply the set of functions used to generate those results, which can be reused
or adapted for projects with similar objectives.

## Installation

To install a local instance of the package, run:

```r
# If you do not have the devtools package,
# install it by uncommenting the following line:
#install.packages(devtools)
devtools::install_github("josefmana/demcrit")
```

## How to Use

There are two main ways to work with the package (each described in its own vignette):

1. **With raw data available** –
Place the raw data files in the data-raw folder and run the included targets pipeline.
See the vignette ["Targets Pipeline"](articles/use1.html) for details.

2. **Without raw data** –
You can still use many of the package functions with your own, similarly structured
dataset. See the vignette ["Out of Pipeline"](articles/use2.html) for guidance.
