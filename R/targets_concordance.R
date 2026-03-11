targets_concordance <- list(
  targets::tar_target(
    name = concordance_statistics, # Describe concordance between different PDD algorithms
    command = describe_concordance(pdd_data)
  ),
  targets::tar_target(
    name = kappa_summmaries, # Extract summaries of Cohen's kappa for the manuscript
    command = summarise_kappa(
      algorithms = algorithms,
      concordance = concordance_statistics$table
    )
  ),
  targets::tar_target(
    name = concordance_statistics_complete_obs, # Describe concordance using complete cases
    command = describe_concordance(
      pdd_data$PDD |>
        drop_incomplete()
    )
  ),
  targets::tar_target(
    name = kappa_summmaries_complete_obs, # Extract summaries of Cohen's kappa using complete cases
    command = summarise_kappa(
      algorithms = algorithms,
      concordance = concordance_statistics_complete_obs$table
    )
  )
)
