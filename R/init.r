.onLoad <- function(libname, pkgname) {
  # Load the tidyverse package
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The tidyverse package must be installed to use this package.")
  }
}
