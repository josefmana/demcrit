#' Check names of the hand-written data.
#'
#' This function compares patients' names
#' in the hand-written data and REDCap.
#'
#' @param d Data set with names to be checked.
#' @param nms Data set with proper names according
#' to REDCap.
#'
#' @returns Nothing, only prints a message with names
#' discrepancies. Ought to be run within the \code{prepare_data}
#' function where it could be made silent.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#'
#' # Checks names:
#' data <- prepare_data(p)
#' data <- prepare_data(p, check.names = T)
#'
#' # Does not check names:
#' data <- prepare_data(p, check.names = F)
#' }
check_names <- function(d, nms) {
  nms <-
    nms |>
    filter(study_id %in% d$id) |>
    column_to_rownames('study_id') |>
    select(jmeno, prijmeni) |>
    mutate_all(~make_clean_names(., allow_dupes = T))
  tnam <-
    sapply(
      rownames(nms),
      function(i) c(
        forname = nms[i, 'jmeno']    == subset(d, id == i & incl == 1)$firstname,
        surname = nms[i, 'prijmeni'] == subset(d, id == i & incl == 1)$surname
      )
    ) |>
    t()
  discid <- rownames(tnam[(!tnam[ ,1] | !tnam[ ,2]), ])
  disctab <-
    left_join(
      nms[discid, ] |> rownames_to_column('id'),
      d[d$id %in% discid, c('id','firstname','surname')],
      by = 'id'
    ) |>
    mutate(
      reason = if_else(
        condition = id == 'IPN143',
        true      = 'married',
        false     = 'typo'
      )
    )
  cat('Please, check whether the reasons of name discrepancies
of patients below are valid. If not, revise the data.\n\n')
  print(disctab)
}
