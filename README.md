
Some validation of PDD diagnoses criteria at level-I.

The text was prepared in Quarto using the [*apaquarto*](https://github.com/wjschne/apaquarto.git) extension.
It ought to be cloned to `_extensions/` folder

Since we work with sensitive clinical information, no data are shared.
For the code to work, the data need to be put into `_raw/` folder and include:

```
PDD_cr1t2.0.csv
ITEMPO_DATA_2024-01-17_1153.csv
ITEMPO-ManaExportNeuropsych_DATA_2024-12-17_1821.csv
```

To create a reproducible environment for the R project we use the [renv](https://rstudio.github.io/renv/) package.
After installing, run the following to (re-)install specific package versions used in this project:

```
# install.packages("renv")
renv::restore()
```

