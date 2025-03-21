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
  disc <- check_compatibility(d1 = ItemData, d2 = REDCap)

  # If there are incompatibilities, print a message informing what data are used:
  if (nrow(disc) > 1) {
    discfile <- here('temp', 'discrepancies.csv')
    cat(paste0('!!! There were some discrepancies, REDCap data were used !!!\nCheck the ', discfile,' file\nto reconcile any incompatibilities.'))
    if (!dir.exists('temp')) dir.create('temp')
    write_csv(disc, discfile, na = '')
  } else {
    if (dir.exists('temp')) unlink('temp', recursive = T)
  }

  # Prepare a full data set:
  df <-
    ItemData |>
    left_join(REDCap, by = 'id', suffix = c('_iw', '_rc')) |>
    rename_at(vars(ends_with('_rc')), ~sub('_rc', '', .x)) |>
    select(!ends_with('_iw')) |>
    filter(incl == 1)

  # Do sanity check of all response data:
  mist <- check_ranges(df)
  if (mist$stop) {
    for (i in names(mist$typos)) {
      if (nrow(mist$typos[[i]] == 0)) {
        mist$typos[[i]] <- NULL
      }
    }
    print(mist$typos)
    stop('There seem to be typos in the data, check the data listed above.')
  }

  # Return the data set:
  df

}
