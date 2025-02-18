# This script runs targets pipeline of the R section of the project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  
  packages = c(
    
    "here",      # for path listing
    "tidyverse", # for data wrangling
    "janitor",   # for cleaning names of diacritics
    "psych"      # for Cohen's Kappa calculation
    
  )
)

# Load all in-house functions:
tar_source()

# List the targets:
list(
  
  # FILES PREP ----
  tar_target(
    name    = outcome_path, # item-specific data
    command = here("_raw", "PDD_cr1t2.0.csv"),
    format  = "file"
  ),
  tar_target(
    name    = meta_path, # meta-data
    command = here("_raw","ITEMPO-ManaExportNeuropsych_DATA_2024-12-17_1821.csv"),
    format  = "file"
  ),
  tar_target(
    name    = id_path, # patients' IDs
    command = here("_raw", "ITEMPO_DATA_2024-01-17_1153.csv"),
    format  = "file"
  ),
  tar_target(
    name    = scoring_path, # tests' scoring
    command = here("helpers","test_scoring.csv"),
    format  = "file"
  ),
  
  # DATA IMPORT ----
  tar_target(
    name    = outcome_file, # item-specific data
    command = read.csv(outcome_path, sep = ";")
  ),
  tar_target(
    name     = meta_file, # meta-data
    command = read.csv(meta_path, sep = ",")
  ),
  tar_target(
    name    = IDs, # patients' IDs
    command = read.csv(id_path, sep = ",")
  ),
  tar_target(
    name    = scoring, # tests' scoring
    command = read.csv(scoring_path, sep = ";")
  ),
  tar_target(
    name    = names, # discrepancies between outcome- and meta-data
    command = import_outcome_data(outcome_file, IDs, "names")
  ),
  tar_target(
    name    = outcomes, # outcome item-level data
    command = import_outcome_data(outcome_file, IDs, "data")
  ),
  tar_target(
    name    = metadata, # extract meta-data
    command = import_metadata(meta_file, outcomes, scoring)
  ),
  tar_target(
    name    = discrepancies, # discrepancies, ought to be empty
    command = compatibility_check(outcomes, metadata)
  ),
  tar_target(
    name    = data, # merge outcome item-level data and meta-data
    command = merge_data(outcomes, metadata)
  ),
  
  # DIAGNOSES DATA EXTRATRACTION ----
  tar_target(
    name    = specifications, # prepare all criteria specifications to be examined
    command = specify_criteria()
  ),
  tar_target(
    name    = pdd_data, # long-format PDD data for IRT modelling
    command = iterate_pdd(data, specifications, format = "long")
  ),
  tar_target(
    name    = pdd_matrix, # wide-format PDD data for confusion matrixes
    command = iterate_pdd(data, specifications, format = "wide")
  ),
  
  # CONFUSION MATRIXES & ASSOCIATION MEASURES ----
  tar_target(
    name    = kappas, # compute pairwise Cohen's kappas
    command = calculate_kappa(pdd_matrix)
  )

)
