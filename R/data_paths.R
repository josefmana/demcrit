#' Define paths to data.
#'
#' This function prepares a set of paths to data
#' that will be used in the analysis. The naming
#' conventions are set and the function will inform
#' the user if the data in the folder of interest ain't
#' compatible with the script.
#' To be then pushed to \code{prepare_data} function
#' for pre-processing.
#'
#' @param dir Input directory where data reside,
#' defaults to dir = 'data-raw'.
#'
#' @returns A character vector with paths towards
#' all raw data used in this study.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#' }
#' @export
data_paths <- function(dir = 'data-raw') {
  dfs <- data.frame(
    type = c(
      'item',
      'demo',
      'meta',
      'scoring',
      'variables'
    ),
    file = c(
      'ItemData.csv', # Item-wise data (originally 'Level1critPDD2.0.csv')
      'REDCapData.csv', # Demography (originally 'ITEMPO-ManaExportNeuropsych_DATA_2024-12-17_1821.csv')
      'MetaData.csv', # Meta-data (originally 'ITEMPO_DATA_2024-01-17_1153.csv')
      'TestScoring.csv', # Scoring of psychological variables
      'VariablesOfInterest.csv' # Variables to be printed in tables
    ),
    message = c(
      paste0('\nItem-wise data is missing or mislabelled!\nInsert a valid file "ItemData.csv" into the /',dir,' folder!\n'),
      paste0('\nREDCap data is missing or mislabelled!\nInsert a valid file "REDCapData.csv" into the /',dir,' folder!\n'),
      paste0('\nPatient identification data is missing or mislabelled!\nInsert a valid file "MetaData.csv" into the /',dir,' folder!\n'),
      paste0('\nFile with test scoring data is missing or mislabelled!\nInsert a valid file "TestScoring.csv" into the /',dir,' folder!\n'),
      paste0('\nFile with variable labels is missing or mislabelled!\nInsert a valid file "VariablesOfInterest.csv" into the /',dir,' folder!\n')
    )
  ) |>
    mutate(path = here::here(dir, file))
  proceed <- T
  for (i in seq_len(nrow(dfs))) {
    if (!file.exists(dfs[i, 'path'])) {
      cat(dfs[i, 'message'])
      proceed <- F
    }
  }
  message('\n')
  stopifnot('\nThe pipeline was terminated due to one or more data files being absent.
Provide valid data files before marching on.' = proceed)
  dfs$path
}
