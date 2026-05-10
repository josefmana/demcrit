# Validation of Algorithms for PDD

This package is **not** intended for broad public use. Its main purpose
is to:

1.  provide transparent documentation of the data analyses forming the
    backbone of the empirical part of *Mana et al. (under review)*, and

2.  supply the set of functions used to generate those results, which
    can be reused or adapted for projects with similar objectives.

## Installation

To install a local instance of the package, run:

``` r

# If you do not have the devtools package,
# install it by uncommenting the following line:
#install.packages(devtools)
devtools::install_deps()
devtools::install_github("josefmana/demcrit")
```

## How to Use

There are two main ways to work with the package (each described in its
own vignette):

1.  **With raw data available** – Place the raw data files in the
    data-raw folder and run the included targets pipeline. See the
    vignette [“Targets
    Pipeline”](https://josefmana.github.io/demcrit/articles/use1.md) for
    details.

2.  **Without raw data** – You can still use many of the package
    functions with your own, similarly structured dataset. See the
    vignette [“Out of
    Pipeline”](https://josefmana.github.io/demcrit/articles/use2.md) for
    guidance.

## Future Directions

> Currently, *demcrit* is designed around the dataset used in our
> original study. As such, it primarily supports direct replications
> rather than general applications.
>
> If you find the approach useful and would like to adapt it for your
> data, feel free to reach out — we are happy to discuss possible
> extensions. We also welcome reports of bugs or mistakes you may
> encounter.
>
> At the moment, explanation and proper integration of the newest
> analysis (feature selection exploraton of screening indexes for
> comprehensive battery dementia classification) is on a to do. It was
> fully implemented into the targets pipeline and added to the
> manuscript, however, integrating it into the package documentation
> seems to be a bit cumbersome.
>
> If I ever get to it and have a good reason to do so, I may trasnform
> *demcrit* to a “proper” R package {demcritr}. Most likely not though.
