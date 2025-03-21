#' Pre-process data.
#'
#' Using data paths, this functions check their
#' format and compatibility and if compatible,
#' the function proceeds to pre-process data.
#'
#' @param p Paths to data. Should be generated
#' by \code{data_paths}.
#'
#' @returns A list with something in it.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#' }
#' @export
prepare_data <- function(p, check.names = T) {

  # Read data sets:
  ItemData <- import_item_data(p[1])
  Scoring  <- readr::read_delim(p[4], delim = ";", col_types = cols(rev = 'c'))
  REDCap   <- import_redcap_data(p[2], Scoring)

  # Check names in the item data:
  if(check.names) check_names(d = ItemData, nms = read_csv(p[3], show_col_types = F))

  # Run a compatibility check:

}
