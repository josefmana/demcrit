targets_data <- list(

  tarchetypes::tar_files(
    name = inputs, # Monitor input data files
    command = data_paths("data-raw"),
    format = "file"
  ),

  targets::tar_target(
    name = variables, # List variables for future use
    command = here::here("data-raw", "VariablesOfInterest.csv"),
    format = "file"
  ),

  targets::tar_target(
    name = raw_data, # Pre-process input data
    command = prepare_data(inputs)
  ),

  targets::tar_target(
    name = pdd_data, # Assign probable PDD
    command = diagnose_pdd_sample(raw_data)
  )
)
