# Use \#1: Targets Pipeline

> The full use of the *demcrit* package relies on patient data that
> cannot be shared publicly for privacy reasons. However, the package
> can be executed on the data owner’s computer ([Laboratory of
> Neurosychology](https://www.vfn.cz/en/pacienti/kliniky-ustavy/neurologicka-klinika/odborne-ambulance/#)
> at the First Faculty of Medicine, Charles University, and the General
> University Hospital in Prague).

## Set-up

Before starting, ensure that you have a local installation of the
*demcrit* R package. If not, see the [package
overview](https://josefmana.github.io/demcrit/index.md) for installation
instructions, or visit the package website’s [reference
list](https://josefmana.github.io/demcrit/reference/index.html) for
function documentation.

Load the package if you wish to access the documentation directly in
RStudio:

``` r
library(demcrit)
```

Next, clone the [*demcrit*
repository](https://github.com/josefmana/demcrit.git) to a local folder
of your choice (here referred to as `studyfolder`), and install the
necessary dependencies:

``` r
devtools::install_deps()
```

Ensure that all required data files are present in the
`studyfolder/raw-data` directory.

## Data Checking

The pipeline first runs the
[`data_paths()`](https://josefmana.github.io/demcrit/reference/data_paths.md)
function, which **checks for missing files** and **naming
inconsistencies**:

``` r
help("data_paths")
```

Additional functions help prevent data-entry errors or structural
inconsistencies before analysis:

``` r
help("check_compatibility") # Looks for incompatibilities between raw item data specific for this project and REDCap data from a larger database.
help("check_names") # Looks for inconsistencies in patients names in the raw item data compare to REDCap databes.
help("check_ranges") # Looks for impossible test scores.
```

## Running the Pipeline

Finally, make sure that the
[apaquarto](https://github.com/wjschne/apaquarto.git) extension (version
4.4.1) is installed in the `studyfolder/_manuscript` directory.

You can then run the full pipeline (including manuscript writing) via:

``` r
#install.packages("targets")
#install.packages("tarchetypes")
targets::tar_make()
```

To visualize the workflow, use:

``` r
targets::tar_visnetwork() # full workflow
targets::tar_glimpse() # simplified workflow (targets only)
```
