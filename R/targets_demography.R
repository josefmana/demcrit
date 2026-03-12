targets_demography <- list(

  # Regress probable PDD on demographics
  targets::tar_target(
    name = demographic_predictors,
    command = regress_pdd_on_demographics(
      d0 = raw_data,
      d1 = pdd_data$PDD
    )
  ),
  # adjusting for neuropsychiatry per Reviewer 2's demand
  targets::tar_target(
    name = demographic_predictors_neuropsychiatry_adjusted,
    command = regress_pdd_on_demographics(
      d0 = raw_data,
      d1 = pdd_data$PDD,
      covs = c("bdi", "stai_1"),
      inter = FALSE
    )
  ),
  # adjusting as per Cereda et al. (2016)
  targets::tar_target(
    name = demographic_predictors_cereda_adjusted,
    command = regress_pdd_on_demographics(
      d0 = raw_data,
      d1 = pdd_data$PDD,
      covs = c("pd_dur", "edu_years"),
      inter = FALSE
    )
  )
)
