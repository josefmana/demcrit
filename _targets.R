library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    "here",      # for path listing
    "tidyverse", # for data wrangling
    "janitor",   # for cleaning names of diacritics
    "psych",     # for Cohen's Kappa calculation
    "caret",     # for confusion matrixes
    "gt"         # for tableing
  )
)

# Load all in-house functions:
tar_source()

# List the targets:
list(
  tar_files(
    name = inputs, # Monitor input data files
    command = data_paths("data-raw"),
    format  = "file"
  ),
  tar_target(
    name = raw_data,
    command = prepare_data(inputs)
  ),
  tar_target(
    name    = pdd_data,
    command = diagnose_pdd_sample(raw_data)
  ),
  tar_target(
    name = variables,
    command = here::here("data-raw","VariablesOfInterest.csv"),
    format = "file"
  ),
  tar_target(
    name = sample_description,
    command = compute_descriptives(raw_data, variables)
  ),
  tar_target(
    name = prevalence_summaries,
    command = summarise_prevalence(pdd_data, variables)
  ),
  tar_target(
    name = demographic_predictors,
    command = regress_pdd_on_demographics(raw_data, pdd_data$PDD)
  ),
  tar_target(
    name = concordance_statistics,
    command = describe_concordance(pdd_data)
  ),
  NULL
)
