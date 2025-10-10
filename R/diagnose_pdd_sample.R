#' Diagnose All Patients
#'
#' Applies all available diagnostic algorithms for probable Parkinson’s disease
#' dementia (PDD) across the full dataset. The set of criteria is generated internally
#' via the \code{specify_algorithms} function. Each patient is assessed using each
#' specification, and the corresponding diagnoses and supporting information are recorded.
#'
#' @param d0 A tibble containing the dataset prepared using \code{prepare_data}.
#'
#' @returns A list with two tibbles:
#' \describe{
#'   \item{\code{algorithms}}{A tibble containing the full specification of all algorithms used.}
#'   \item{\code{diagnoses}}{A tibble containing:
#'     \itemize{
#'       \item Patient ID
#'       \item Criterion label (linked to the specification)
#'       \item PDD diagnosis (`TRUE`/`FALSE`)
#'       \item Individual diagnostic flags corresponding to Table 2 in Dubois et al. [(2007)](https://doi.org/10.1002/mds.21844)
#'       \item Impairment labels across cognitive domains (per Level I criteria)
#'     }}
#' }
#'
#' @seealso
#' * [prepare_data()] prepares `d0`.
#' * [specify_algorithms()] runs internally to prepare a set of algorithms to be used.
#' * [diagnose_pdd_case()] runs internally to diagnose patients using a single algorithm.
#'
#' @references
#' Dubois, B., Burn, D., Goetz, C., Aarsland, D., Brown, R. G., Broe, G. A., ... & Emre, M. (2007).
#'    Diagnostic procedures for Parkinson's disease dementia: recommendations from the movement disorder
#'    society task force. Movement disorders, 22(16), 2314-2324. \doi{10.1002/mds.21844}
#'
#' @examples
#' \dontrun{
#' p <- data_paths("data-raw")
#' data <- prepare_data(p)
#' pdd  <- diagnose_pdd_sample(data)
#' }
#'
#' @export
diagnose_pdd_sample <- function(d0) {
  algos <- specify_algorithms()
  pdd <- purrr::map_dfr(seq_len(nrow(algos)), function(i) {
    diagnose_pdd_case(d0, algos[i, ]) |>
      dplyr::mutate(type = algos$type[i], .after = id)
  })
  # Return both algorithms specification and PDD diagnoses:
  list(algorithms = algos, PDD = pdd)
}
