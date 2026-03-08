#' Perform variable selection
#'
#' A wrapper for [projpred::cv_varsel()]. Using a provided reference model,
#' runs a projective prediction feature selection via cross-validation.
#'
#' @param refm_fit Reference model, e.g., fitted via \code{fit_reference}.
#' @param ... Parameters passed to [projpred::cv_varsel()].
#'
#' @returns An object of class vsel as returned by [projpred::cv_varsel()].
#'
#' @seealso
#' * [fit_reference()]
#' * [projpred::cv_varsel()]
#'
#' @export
perform_varsel <- function(refm_fit, ...) {
  refm_obj <- projpred::get_refmodel(refm_fit)
  projpred::cv_varsel(refm_obj, ... )
}
