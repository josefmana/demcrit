library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    'here',      # for path listing
    'tidyverse', # for data wrangling
    'janitor',   # for cleaning names of diacritics
    'psych',     # for Cohen's Kappa calculation
    'caret',     # for confusion matrixes
    'gt'         # for tableing
  )
)

# Load all in-house functions:
tar_source()

# List the targets:
list(
  tar_files(
    name    = inputs, # Monitor input data files
    command = data_paths('data-raw'),
    format  = 'file'
  ),
  tar_target(
    name    = raw_data,
    command = prepare_data(inputs)
  ),
  tar_target(
    name    = pdd_data,
    command = diagnose_pdd_sample(raw_data)
  ),
  tar_target(
    name    = sample_description,
    command = compute_descriptives(raw_data, here::here('data-raw','VariablesOfInterest.csv'))
  ),
  tar_target(
    name    = prevalence_summaries,
    command = summarise_prevalence(pdd_data, here::here('data-raw','VariablesOfInterest.csv'))
  ),
  # CONFUSION MATRIXES & ASSOCIATION MEASURES ----
  #tar_target(
  #  name    = kappas, # compute pairwise Cohen's kappas
  #  command = calculate_kappa(pdd_matrix)
  #),
  #tar_target(
  #  name    = confusion_matrices, # compute pairwise confusion matrixes
  #  command = calculate_confusion(data = pdd_matrix, kappas = kappas)
  #),
  #tar_target(
  #  name    = confusion_plots, # plot all pairwise confusion matrixes
  #  command = plot_confusion(mat = confusion_matrices)
  #),
  NULL
)
