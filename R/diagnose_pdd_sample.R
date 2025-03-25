#' Diagnose PDD using all available criteria
#' in all patients.
#'
#' This function takes in study data and loops
#' through all criteria specifications (generated
#' internally via the \code{specify_criteria}
#' function) and patients to arrive at PDD
#' diagnosis.
#'
#' @param d0 A tibble with the data generated
#' by \code{prepare_data}
#'
#' @returns A list containing two tibbles with
#' (i) criteria specification and (ii) patient id,
#' criterion type (to be mapped to criteria specification),
#' PDD diagnosis, single criteria akin to Table 2 of
#' Dubois et al. (2007) and impairment statements for
#' each cognitive domain according to Level I.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#' pdd  <- diagnose_pdd_sample(data)
#' }
#'
#' @export
diagnose_pdd_sample <- function(d0) {
  crit <- specify_criteria()
  pdd  <- lapply(
    seq_len(nrow(crit)),
    function(i)
      diagnose_pdd_case(d0, crit[i, ]) |>
      mutate(type = crit$type[i], .after = id)
  ) |>
    reduce(full_join) |>
    suppressMessages()
  # Return both criteria specification and PDD diagnoses:
  list(criteria = crit, PDD = pdd)
}
