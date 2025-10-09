# Validation of probable PDD algorithms

This package is not intended for public use. The idea is to provide a documentation
behind data analysis that serves as a backbone of the empirical part of the paper
Mana et al. (under review)^[Will be updated once the manuscript is accepted for
publication.].

Furthermore, if we are able to share some of the data in the future, we will use
this package to document processes that can be used to reproduce our results (since
sharing raw patients data is out of question due to privacy concerns, the pre-processing
pipeline presented here will be functional only on data owner's computer at the First
Faculty of Medicine, Charles University).

To start, ensure you have the correct dependencies:

```r
# clone the repository, open it in R
# install missing dependencies, if any
devtools::install_deps()
```
If you have the raw data at your disposal, make sure they are present in the
`data-raw` folder (and correctly named, although, if they are not named correctly,
the package will help you find the mistakes).

Next, make you sure you have the [apaquarto](https://github.com/wjschne/apaquarto.git)
extension (version 4.4.1) installed in the `_manuscript` folder. You can then run the
full pipeline via:

```r
# if you wanted to use the functions independently,
# you can source them
#tar_source()
targets::tar_make()
```

Usage out of the pipeline will be sorted out later, once we know which (pre-processsed)
data can be shared ...
