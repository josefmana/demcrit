#' Perform projective prediction
#'
#' Given a vsel variable selection object and size of the projected model,
#' projects posterior of the reference model onto the selected submodel
#' parameter space.
#'
#' @param vs A vsel object.
#' @param n_chosen Number of parameters to retain in the model.
#' @param seed Seed for reproducibility of posterior prediction.
#' @param ... Parameters passed to [projpred::project()].
#'
#' @returns List containing:
#' \describe{
#'   \item{model}{Projected model}
#'   \item{ppred}{Posterior predictions from the model}
#'   \item{probs}{Expected posterior probabilities for data used to fit the model}
#' }
#'
#' @export
perform_projpred <- function(vs, n_chosen = NULL, seed = NA, ...) {

  proj_model <- projpred::project(vs, nterms = n_chosen, ...)
  ppred <- projpred::proj_predict(proj_model, .seed = seed)
  probs <- colMeans(ppred)

  list(
    model = proj_model,
    ppred = ppred,
    probs = probs
  )
}
