targets_prediction <- list(

  # Fit reference models and run variable selection
  targets::tar_target(
    name = prediction_models,
    command = grid_models(
      d = pdd_data$PDD,
      gss = c("Lvl.II (1)", "Lvl.II (1)", "Lvl.II (2)"),
      N = c(4, 5, 4),
      n_chosen = c(2, 2, 0),
      bind = TRUE,
      chains = 4,
      cores = 4,
      warmup = 1000,
      iter = 3500,
      seed = 12345
    )
  ),

  targets::tar_target(
    name = index_correlations,
    command = correlate_indexes(
      d0 = prediction_models$reference[[1]]$data,
      method = "spearman",
      ideal = NULL # default "ideal" comparisons
    )
  ),

  targets::tar_target(
    name = scoring_rules,
    command = purrr::map(c(1, 2), function(i) {
      run_scoring_rule_pipeline(
        y_obs = prediction_models$reference[[i]]$data$y,
        model = prediction_models$projection[[i]], # retaining Lvl. II (1) only
        scaling = prediction_models$reference[[i]]$scaling,
        linear = TRUE,
        nms = c("Intercept", "MoCA Five words", "MMSE Sevens"),
        prevs = seq(0.1, 0.5, by = 0.1)
      )
    })
  ),

  targets::tar_target(
    name = posterior_prediction,
    command = grid_postpred(prediction_models$projection[[1]])
  )
)
