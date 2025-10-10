#' Define Raw Data Paths
#'
#' Prepares a set of file paths pointing to all required raw data files used in the analysis.
#' The function enforces naming conventions and validates compatibility between the contents
#' of the specified directory and the expected structure. It is intended to be used as input
#' for the \code{prepare_data} function.
#'
#' @param dir A character string specifying the input directory where raw data files reside.
#'    Defaults to \code{"data-raw"}.
#'
#' @returns A named character vector with file paths to all required raw data files.
#'
#' @seealso [prepare_data()] is the next step in analysis pipeline.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths("data-raw")
#' data <- prepare_data(p)
#' }
#' @export
data_paths <- function(dir = "data-raw") {
  dfs <- data.frame(
    type = c(
      "item",
      "demo",
      "meta",
      "scoring",
      "variables"
    ),
    file = c(
      "ItemData.csv", # Item-wise data (originally "Level1critPDD2.0.csv")
      "REDCapData.csv", # Demography (originally "ITEMPO-ManaExportNeuropsych_DATA_2024-12-17_1821.csv")
      "MetaData.csv", # Meta-data (originally "ITEMPO_DATA_2024-01-17_1153.csv")
      "TestScoring.csv", # Scoring of psychological variables
      "VariablesOfInterest.csv" # Variables to be printed in tables
    ),
    message = c(
      glue::glue("\nItem-wise data is missing or mislabelled!\nInsert a valid file 'ItemData.csv' into the /{dir} folder!\n"),
      glue::glue("\nREDCap data is missing or mislabelled!\nInsert a valid file 'REDCapData.csv' into the /{dir} folder!\n"),
      glue::glue("\nPatient identification data is missing or mislabelled!\nInsert a valid file 'MetaData.csv' into the /{dir} folder!\n"),
      glue::glue("\nFile with test scoring data is missing or mislabelled!\nInsert a valid file 'TestScoring.csv' into the /{dir} folder!\n"),
      glue::glue("\nFile with variable labels is missing or mislabelled!\nInsert a valid file 'VariablesOfInterest.csv' into the /{dir} folder!\n")
    )
  ) |>
    dplyr::mutate(path = here::here(dir, file))
  proceed <- TRUE
  for (i in seq_len(nrow(dfs))) {
    if (!file.exists(dfs[i, "path"])) {
      cat(dfs[i, "message"])
      proceed <- FALSE
    }
  }
  stopifnot("\nThe pipeline was terminated due to one or more data files being absent.
Provide valid data files before marching on." = proceed)
  dfs$path
}
