#' Derive a scoring rule for PDD
#'
#' Given a prediction linear logistic model and its ROC analysis results,
#' computes a scoring rule for deciding a patient suffers probable PDD given
#' screening criteria.
#'
#' @param pt The threshold on probability scale. E.g., derived from
#'   \code{run_roc}.
#' @param coefs Model coefficients as computed by \code{extract_coefficients()}.
#' @param inverse Should `pt` be inversed from probability to logit scale?
#' @param ... Unused. There to allow for compatibility with
#'   \code{run_scoring_rule_pipeline.}
#'
#'@returns List containing:
#'   - score coefficients
#'   - cutoff
#'   - equation (character)
#'   - integer scoring rule
#'
#' @export
derive_scoring_rule <- function(
    pt,
    coefs,
    inverse = TRUE,
    ...
) {

  if (inverse) {
    logit_pt <- qlogis(pt)
  } else {
    logit_pt <- pt
  }
  b0 <- coefs[1]
  b <- coefs[-1]

  vars <- names(b)
  rhs <- logit_pt - b0

  score_coefs <- -b
  score_cutoff <- -rhs

  eq <- paste(
    paste(sprintf("%.3f × %s", score_coefs, vars), collapse = " + "),
    sprintf("≤ %.3f", score_cutoff)
  )

  scale_factor <- 1 / min(abs(score_coefs))
  int_coefs <- round(score_coefs * scale_factor, 2)
  int_cutoff <- round(score_cutoff * scale_factor, 2)

  int_eq <- paste(
    paste(sprintf("%.2f × %s", int_coefs, vars), collapse = " + "),
    sprintf("≤ %.2f", int_cutoff)
  )

  list(
    score_coefs = -b,
    score_cutoff = -rhs,
    equation = eq,
    integer_rule = list(
      coefs = int_coefs,
      cutoff = int_cutoff,
      equation = int_eq
    )
  )
}
