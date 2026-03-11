targets_description <- list(

  targets::tar_target(
    name = sample_description, # Describe the sample
    command = compute_descriptives(raw_data, variables)
  ),

  targets::tar_target(
    name = rate_summaries, # Summarise PDD rates
    command = summarise_rates(pdd_data, variables)
  ),

  targets::tar_target(
    name = algorithms, # List all algorithms used in the study
    command = list_algorithms(rate_summaries$table)
  )
)
