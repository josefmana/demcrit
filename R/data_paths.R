#' Define paths to data.
#'
#' This function prepares a set of paths to
#' data that will be used in the analysis. The naming
#' conventions are set and the function will inform
#' the user if the data in the folder of interest ain't
#' compatible with the script.
#' To be then pushed to \code{prepare_data} function
#' for pre-processing.
#'
#' @param dir Input directory where data reside, default dir = 'data-raw'.
#' @returns A list with something in it.
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- import_data(p)
#' }
#' @export
data_paths <- function(dir = 'data-raw') {

  # List all data files needed for the study:
  dfs <- data.frame(
    type = c(
      'item',
      'demo',
      'meta',
      'scoring'
    ),
    file = c(
      'ItemData.csv', # Item-wise data (originally 'Level1critPDD2.0.csv')
      'REDCapData.csv', # Demography (originally 'ITEMPO-ManaExportNeuropsych_DATA_2024-12-17_1821.csv')
      'MetaData.csv', # Meta-data (originally 'ITEMPO_DATA_2024-01-17_1153.csv')
      'TestScoring.csv' # Scoring of psychological variables
    ),
    message = c(
      paste0('\nItem-wise data is missing or mislabelled!\nInsert a valid file "ItemData.csv" into the /',dir,' folder!'),
      paste0('\nREDCap data is missing or mislabelled!\nInsert a valid file "REDCapData.csv" into the /',dir,' folder!'),
      paste0('\nPatient identification data is missing or mislabelled!\nInsert a valid file "MetaData.csv" into the /',dir,' folder!'),
      paste0('\nFile with test scoring data is missing or mislabelled!\nInsert a valid file "TestScoring.csv" into the /',dir,' folder!')
    )
  ) |> mutate(path = here::here(dir, file))

  # Check whether the data exist:
  proceed <- T
  for (i in seq_len(nrow(dfs))) {
    if (!file.exists(dfs[i, 'path'])) {
      message(dfs[i, 'message'])
      proceed <- F
    }
  }
  message('\n')
  stopifnot('\nThe pipeline was terminated due to one or more data files being absent.\nProvide valid data files before marching on.' = proceed) |> try()

  # Return paths to data if the files are present:
  dfs$path |> `names<-`(dfs$type)

}
