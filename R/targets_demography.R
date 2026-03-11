targets_demography <- list(

  targets::tar_target(
    name = demographic_predictors, # Regress probable PDD on demographics
    command = regress_pdd_on_demographics(
      d0 = raw_data,
      d1 = pdd_data$PDD
    )
  ),

  targets::tar_target(
    name = demographic_predictors_neuropsychiatry_adjusted, # Regress probable PDD on demographics adjusting for neuropsychiatry per Reviewer 2's demand
    command = regress_pdd_on_demographics(
      d0 = raw_data,
      d1 = pdd_data$PDD,
      covs = c("bdi", "stai_1"),
      inter = FALSE
    )
  ),

  targets::tar_target(
    name = demographic_predictors_cereda_adjusted, # Regress probable PDD on demographics adjusting as per Cereda et al. (2016)
    command = regress_pdd_on_demographics(
      d0 = raw_data,
      d1 = pdd_data$PDD,
      covs = c("pd_dur", "edu_years"),
      inter = FALSE
    )
  )
)
