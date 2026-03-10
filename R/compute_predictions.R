#' Compute model predictions
#'
#' Given a Bayesian "projpred" or "brms" model, computes linear and full
#' posterior predicitons.
#'
#' @param model The model
#' @param seed Optional seed for reproducibility
#' @param ... Unused. There to allow for compatibility with \code{run_scoring_rule_pipeline}.
#'
#' @returns Tibble with two columns:
#' \describe{
#'   \item{epred}{Linear prediction on the logit scale}
#'   \item{ppred}{Posterior prediction on the outcome scale}
#' }
#'
#' @export
compute_predictions <- function(model, seed = NULL, ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (inherits(model, "projection")) {
    epred <- projpred::proj_linpred(model)$pred
    ppred <- projpred::proj_predict(model)
  } else if (inherits(model, "brmsfit")) {
    epred <- brms::posterior_epred(model)
    ppred <- brms::posterior_predict(model)
  }

  tibble::tibble(
   epred = epred,
   ppred = ppred
  )
}
