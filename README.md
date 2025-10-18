# Validation of Algorithms for PDD

This package is not intended for a widespread public use. The idea is to:

1. provide a documentation behind data analysis that serves as a backbone
of the empirical part of the paper Mana et al. (under review), and

2. provide a set of functions used to generate our results that could be
directly used or slightly adjusted for projects aiming at similar goals.

To start, install a local instance of the package:

```r
# If you do not have the devtools package,
# install it by uncommenting the following line:
#install.packages(devtools)
devtools::install_github("josefmana/demcrit")
```

There are two ways to use the package (both of which will receive their own vignette
in the near future):

1. If you have the raw data at your disposal, make sure they are present in the
`data-raw` folder and run the `targets` pipeline which is part of the package
(this option is only possible from the First Faculty of Medicine, Charles
University computers because of privacy concerns of patients' data; it also
requires the [apaquarto](https://github.com/wjschne/apaquarto.git)
extension (version 4.4.1) to be installed in the `_manuscript` folder)

2. If you do not have the raw data, you can still use some of the functions on
your own, similarly structured data set. A vignette showing reproducible example
of such a use will be added in the next version of this package.
