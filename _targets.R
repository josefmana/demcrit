library(tidyverse) |> suppressPackageStartupMessages()
library(gt)
library(targets)
library(tarchetypes)

tar_option_set()
tar_source()

list(
  targets_data,
  targets_description,
  targets_demography,
  targets_concordance,
  targets_prediction,

  tarchetypes::tar_render(
    name = manuscript,
    path = here::here("_manuscript", "manuscript.qmd"),
    quiet = FALSE
  ),
  NULL
)
