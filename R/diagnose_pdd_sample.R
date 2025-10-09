#' Diagnose PDD in all patients using all available criteria
#'
#' Applies all available diagnostic criteria for probable Parkinson’s disease
#' dementia (PDD) across the full dataset. The set of criteria is generated internally
#' via the \code{specify_criteria} function. Each patient is assessed using each
#' specification, and the corresponding diagnoses and supporting information are recorded.
#'
#' @param d0 A tibble containing the dataset prepared using \code{prepare_data}.
#'
#' @returns A list with two tibbles:
#' \describe{
#'   \item{\code{criteria}}{A tibble containing the full specification of all criteria used.}
#'   \item{\code{diagnoses}}{A tibble containing:
#'     \itemize{
#'       \item Patient ID
#'       \item Criterion label (linked to the specification)
#'       \item PDD diagnosis (TRUE/FALSE)
#'       \item Individual diagnostic flags corresponding to Table 2 in Dubois et al. (2007)
#'       \item Impairment labels across cognitive domains (per Level I criteria)
#'     }}
#' }
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
  crit <- specify_criteria()
  pdd <- purrr::map_dfr(seq_len(nrow(crit)), function(i) {
    diagnose_pdd_case(d0, crit[i, ]) |>
      dplyr::mutate(type = crit$type[i], .after = id)
  })
  # Return both criteria specification and PDD diagnoses:
  list(criteria = crit, PDD = pdd)
}
