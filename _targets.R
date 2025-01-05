# This script runs targets pipeline of the R section of the project.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  
  packages = c(
    
    "here", # for path listing
    "tidyverse", # for data wrangling
    "janitor", # for cleaning names of diacritics
    "psych" # for Cohen's Kappa calculation
    
  )
)

# Load all in-house functions:
tar_source()

# List the targets:
list(
  
  # FILES PREP ----
  tar_target( outcome_path, list_path("_raw", "PDD_cr1t2.0.csv"), format = "file" ), # item-specific data
  tar_target( meta_path, list_path("_raw","ITEMPO-ManaExportNeuropsych_DATA_2024-12-17_1821.csv"), format = "file" ), # meta-data
  tar_target( id_path, list_path("_raw", "ITEMPO_DATA_2024-01-17_1153.csv"), format = "file" ), # patients' IDs
  tar_target( scoring_path, list_path("helpers","test_scoring.csv"), format = "file" ), # tests' scoring
  
  # DATA IMPORT ----
  tar_target( outcome_file, read_data(outcome_path, ";") ), # item-specific data
  tar_target( meta_file, read_data(meta_path, ",") ), # meta-data
  tar_target( IDs, read_data(id_path, ",") ), # patients' IDs
  tar_target( scoring, read_data(scoring_path, ";") ), # tests' scoring
  
  tar_target( names, import_outcome_data(outcome_file, IDs, "names") ), # discrepancies between outcome- and meta-data
  tar_target( outcomes, import_outcome_data(outcome_file, IDs, "data") ), # outcome item-level data
  tar_target( metadata, import_metadata(meta_file, outcomes, scoring) ), # extract meta-data
  tar_target( discrepancies, compatibility_check(outcomes, metadata) ), # discrepancies, ought to be empty
  tar_target( data, merge_data(outcomes, metadata) ), # merge outcome item-level data and meta-data
  
  # DIAGNOSES DATA EXRATRACTION ----
  tar_target( specifications, specify_criteria() ), # prepare all criteria specifications to be examined
  tar_target( pdd_data, iterate_pdd(data, specifications, format = "long") ), # long-format PDD data for IRT modelling
  tar_target( pdd_matrix, iterate_pdd(data, specifications, format = "wide") ), # wide-format PDD data for confusion matrixes
  
  # CONFUSION MATRIXES & ASSOCIATION MEASURES ----
  tar_target( kappas, calculate_kappa(pdd_matrix) ) # compute pairwise Cohen's kappas

)
