targets_data <- list(

  # Monitor input data files
  tarchetypes::tar_files(
    name = inputs,
    command = data_paths("data-raw"),
    format = "file"
  ),

  # List variables for future use
  targets::tar_target(
    name = variables,
    command = here::here("data-raw", "VariablesOfInterest.csv"),
    format = "file"
  ),

  # Pre-process input data
  targets::tar_target(
    name = raw_data_all,
    command = prepare_data(inputs)
  ),

  # Assign probable PDD
  targets::tar_target(
    name = pdd_data_all,
    command = diagnose_pdd_sample(raw_data_all)
  ),

  # Keep complete cases
  targets::tar_target(
    name = incomplete_cases,
    command = find_incomplete(pdd_data_all$PDD)
  ),
  targets::tar_target(
    name = raw_data,
    command = trim_data(raw_data_all, incomplete_cases)
  ),
  targets::tar_target(
    name = pdd_data,
    command = trim_data(pdd_data_all, incomplete_cases)
  )
)
