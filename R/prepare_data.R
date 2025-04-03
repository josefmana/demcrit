#' Pre-process data.
#'
#' Using data paths, this functions check their
#' format and compatibility and if compatible,
#' the function proceeds to pre-process data.
#'
#' @param p Paths to data. Should be generated
#' by \code{data_paths}.
#'
#' @returns A tibble including relevant variables.
#' It also prints information regarding potential
#' discrepancies in data and stops the analysis
#' in some cases.
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
  # Check names nd compatibility:
  if(check.names) {
    check_names(d = ItemData, nms = read_csv(p[3], show_col_types = F))
  }
  disc <- check_compatibility(d1 = ItemData, d2 = REDCap)
  if (nrow(disc) > 1) {
    discfile <- here('temp', 'discrepancies.csv')
    cat(paste0('\n!!! There were some discrepancies, REDCap data were used !!!\nCheck the ', discfile,' file\nto reconcile any incompatibilities.\n'))
    if (!dir.exists('temp')) dir.create('temp')
    write_csv(disc, discfile, na = '')
  } else {
    if (dir.exists('temp')) unlink('temp', recursive = T)
  }
  # Prepare a full data set:
  df <-
    ItemData |>
    filter(incl == 1) |>
    left_join(REDCap, by = 'id', suffix = c('_iw', '_rc'))
  cv <-
    names(df)[grepl('_iw', names(df))] |>
    sub(x = _, '_iw', '')
  for (i in cv) {
    df[ , i] <- NA
    for (j in seq_len(nrow(df))) {
      df[j , i] <- ifelse(
        test = !is.na(df[j , paste0(i,'_rc')]),
        yes  = df[j , paste0(i,'_rc')],
        no   = df[j , paste0(i,'_iw')]
      )
    }
    df[ , paste0(i,'_rc')] <- NULL
    df[ , paste0(i,'_iw')] <- NULL
  }
  mist <- check_ranges(df)
  if (mist$stop) {
    for (i in names(mist$typos)) {
      if (nrow(mist$typos[[i]] != 0)) {
        print(mist$typos[i])
      }
    }
    stop('There seem to be typos, check the data listed above.')
  }
  # Return the data set:
  df
}
