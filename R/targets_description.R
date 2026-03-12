targets_description <- list(

  # Describe the sample
  targets::tar_target(
    name = sample_description,
    command = compute_descriptives(raw_data, variables)
  ),

  # Summarise PDD rates
  targets::tar_target(
    name = rate_summaries,
    command = summarise_rates(pdd_data, variables)
  ),

  # List all algorithms used in the study
  targets::tar_target(
    name = algorithms,
    command = list_algorithms(rate_summaries$table)
  )
)
