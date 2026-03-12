targets_concordance <- list(

  # Describe concordance between different PDD algorithms
  targets::tar_target(
    name = concordance_statistics_pairwise_obs,
    command = describe_concordance(pdd_data_all)
  ),

  # Extract summaries of Cohen's kappa for the manuscript
  targets::tar_target(
    name = kappa_summaries_pairwise_obs,
    command = summarise_kappa(
      algorithms = algorithms,
      concordance = concordance_statistics_pairwise_obs$table
    )
  ),

  # Describe concordance using complete cases
  targets::tar_target(
    name = concordance_statistics_complete_obs,
    command = describe_concordance(pdd_data)
  ),
  targets::tar_target(
    name = kappa_summaries_complete_obs,
    command = summarise_kappa(
      algorithms = algorithms,
      concordance = concordance_statistics_complete_obs$table
    )
  ),

  # Calculate differences between missing values treatment strategies:
  targets::tar_target(
    name = concordance_differences,
    command = subtract_concordance_matrixes(
      conc0 = concordance_statistics_complete_obs$table,
      conc1 = concordance_statistics_pairwise_obs$table,
      label0 = "complete",
      label1 = "pairwise"
    )
  )
)
