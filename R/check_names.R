#' Check patient names in hand-written data
#'
#' Compares patient names between a manually entered dataset and a reference set of
#' correct names (e.g., from REDCap) to identify any discrepancies.
#'
#' @param d A data frame containing patient names to be checked (e.g., hand-written data).
#' @param nms A data frame or vector containing the correct patient names (e.g., from REDCap).
#'
#' @returns Nothing. Prints a message listing any discrepancies found. Intended to be called
#' within the \code{prepare_data()} function, where the check can optionally be silenced.
#'
#' @examples
#' \dontrun{
#' p <- data_paths("data-raw")
#'
#' # With name checking:
#' data <- prepare_data(p, check.names = TRUE)
#'
#' # Without name checking:
#' data <- prepare_data(p, check.names = FALSE)
#' }
check_names <- function(d, nms) {
  nms <- nms |>
    filter(study_id %in% d$id) |>
    column_to_rownames("study_id") |>
    select(jmeno, prijmeni) |>
    mutate_all(\(x) janitor::make_clean_names(x, allow_dupes = TRUE))
  tnam <- sapply(rownames(nms), function(i) {
    c(forname = nms[i, "jmeno"] == subset(d, id == i & incl == 1)$firstname,
      surname = nms[i, "prijmeni"] == subset(d, id == i & incl == 1)$surname
    )
  }) |>
    t()
  discid <- rownames(tnam[(!tnam[ ,1] | !tnam[ ,2]), ])
  disctab <- left_join(
    nms[discid, ] |> rownames_to_column("id"),
    d[d$id %in% discid, c("id", "firstname", "surname")],
    by = join_by(id)
  ) |>
    mutate(reason = if_else(
      condition = id == "IPN143",
      true = "married",
      false = "typo"
    ))
  if (nrow(disctab) > 0) {
    cat("Please, check whether the reasons of name discrepancies
of patients below are valid. If not, revise the data.\n\n")
    print(disctab)
  }
}
