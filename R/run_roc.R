#' Run ROC analysis
#'
#' Given observed values and predictions, runs ROC analysis.
#'
#' @param y_obs Observed values
#' @param preds Predictions. Does not need to be on the same scale as observed
#'   values. Can be single test scores or expected probabilities from a logistic
#'   regression or any other model.
#' @param prevs Prevalences.
#'
#' @returns List with two components:
#' \describe{
#'   \item{ROC}{List of class "roc", including AUC plot.}
#'   \item{thresholds}{Tibble with the best thresholds according to assumed PDD
#'   prevalences with additional columns for specificity, sensitivity, and
#'   accuracy in current sample.}
#' }
#'
#' @export
run_roc <- function(y_obs, preds, prevs = seq(0.1, 0.5, 0.1)) {

  if (length(y_obs) != length(preds)) {
    cli::cli_abort(c(
            "Data {.code y_obs} and predictions {.code preds} must be of the same length.",
      "i" = "You provided data of length {length(y_obs)}, and predictions of length {length(preds)}"
    ))
  }

  roc_obj <- pROC::roc(y_obs, preds, plot = TRUE, ci = TRUE)
  pt <- purrr::map_dfr(prevs, function(p) {
    pROC::coords(
      roc_obj,
      x = "best",
      ret = c("threshold", "specificity", "sensitivity", "accuracy"),
      best.method = "youden",
      best.weights = c(1, p) # Loop through plausible PDD prevalences
    ) |>
      tibble::add_column(prevalence = p, .before = 1)
  })

  list(
    ROC = roc_obj,
    thresholds = pt
  )
}
