#' Run pipeline deriving scoring rule for PDD
#'
#' Using different assumed PDD prevalences and different models, computes a
#' set of ROC analyses and derives decision rules for the best threshold
#' identified by each.
#'
#' @param y_obs Observed outcome values.
#' @param model Model object used for prediction (needs to be Bayesian and
#'   ammenable to functions from the \{posterior\}package.
#' @param scaling A scaling table with columns "x" for predictor name, "M" for
#'   mean and "SD" for standard deviation..
#' @param linear Should predictions from the linear model (`TRUE`) or on the
#'   response scale (`FALSE`) be used?
#' @param ... Other parameters going either to \code{run_roc} if `prevs`,
#'   to \code{extract_coefficients} if `stat`, to or to \code{compute_predictions}
#'   if `seed`.
#' @param nms Optional coefficient names, including intercepts. Double check
#'   they are in correct order!
#'
#' @returns description
#'
#' @export
run_scoring_rule_pipeline <- function(
    y_obs,
    model,
    scaling,
    linear = TRUE,
    ...,
    nms = NULL
) {

  prds <- compute_predictions(model, ...)
  if (linear) {
    probs <- colMeans(prds$epred)
  } else {
    probs <- colMean(prds$ppred)
  }

  ROC <- run_roc(y_obs, probs, ...)
  coeffs <- extract_coefficients(model, scaling, ...)
  if (!is.null(nms)) {
    names(coeffs) <- nms
  }

  ROC$thresholds |>
    dplyr::mutate(
      scoring = purrr::map(threshold, function(thr) {
        derive_scoring_rule(thr, coeffs, inverse = !linear)
      })
    )
}
