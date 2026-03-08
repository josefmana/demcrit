#' Extract variable selection summaries
#'
#' Given a vsel object, returns basic descriptions of the model to aid
#' variable selection and projective prediction process.
#'
#' @param vs A vsel object.
#'
#' @returns List containing:
#' \describe{
#'   \item{n_suggest}{Number of terms suggested for projection.}
#'   \item{plot_pp}{Plot of predictive performance.}
#'   \item{plot_rk}{Plot of predictors' ranking variability.}
#'   \item{plot_rk_cum}{Plot of predictors' cummulative ranking variability.}
#' }
#'
#' @export
describe_varsel <- function(vs) {
  rk <- projpred::ranking(vs)
  list(
    n_suggest = projpred::suggest_size(vs),
    plot_pp = plot(vs, deltas = TRUE),
    plot_rk = plot(projpred::cv_proportions(rk)),
    plot_rk_cum = plot(projpred::cv_proportions(rk, cumulate = TRUE))
  )
}
