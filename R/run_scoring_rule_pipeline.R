#' Run pipeline deriving scoring rule for PDD
#'
#' Using different assumed PDD prevalences and different models, computes a
#' set of ROC analyses and derives decision rules for the best threshold
#' identified by each.
#'
#' @param model_grid A single row of \code{grid_models}.
#' @param ... Other parameters going either to \code{run_roc} if `prevs` or
#'   to \code{extract_coefficients} if `stat`.
#' @param nms Optional coefficient names, including intercepts. Double check
#'   they are in correct order!
#'
#' @returns description
#'
#' @export
run_scoring_rule_pipeline <- function(model_grid, ..., nms = NULL) {

  y <- model_grid$fit[[1]]$data$y
  s <- model_grid$fit[[1]]$scaling
  m <- model_grid$projection[[1]]$model
  probs <- model_grid$projection[[1]]$probs

  ROC <- run_roc(y, probs, ...)
  c <- extract_coefficients(m, s, ...)
  if (!is.null(nms)) {
    names(c) <- nms
  }

  ROC$thresholds |>
    dplyr::mutate(
      scoring = purrr::map(threshold, function(t) {
        derive_scoring_rule(t, c)
      })
    )
}
