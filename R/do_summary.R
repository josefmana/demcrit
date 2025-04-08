#' Summarises data in one of a set of pre-specified ways.
#'
#' This function computes demographic summaries
#' of the sample including their graphical
#' representation.
#'
#' @param x A vector to be summarised.
#' @param dec An integer denoting decimals.
#' @param sum A type of summary. Takes in NULL (default)
#' for simple printing, 'N' for number of observations,
#' 'msd' for M ± SD, 'M' for mean, SD' for standard deviation,
#' 'Md' for median, 'IQR', for interquartile range,
#' 'minmax' for minimum-maximum, Min' for minimum, 'Max'
#' for maximum, p' for a p-value (for tables), 'ptext'
#' for a p-value (for in-text), 'Nperc' for a N (%)
#' representation of a binary variable, 'Nslash' for a
#' N(group1)/N(group2)/... representation of nominal
#' variable with unlimited number of categories, estCI
#' for an estimate [CI].
#'
#' @returns A summary of choice.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#'
#' M  <- do_summary(data[ , 25], 2, 'M')
#' SD <- do_summary(data[ , 25], 2, 'SD')
#' Md <- do_summary(data[ , 25], 2, 'Md')
#' mm <- do_summary(data[ , 25], 0, 'minmax')
#' }
#' @export
do_summary <- function(x, dec, sum = NULL) {
  # Prepare a functions for printing:
  rprint <- function(x0, dec0) {
    sprintf(paste0('%.',dec0,'f'), round(x0, dec0))
  }
  zerolead <- function(x1, dec1) {
    sub('0.', '.', rprint(x1, dec1), fixed = T)
  }
  # Prepare functions for extracting statistics ignoring NAs:
  M     <- function(x0, ...) mean(x0, na.rm = T, ...)
  Md    <- function(x0, ...) median(x0, na.rm = T, ...)
  SD    <- function(x0, ...) sd(x0, na.rm = T, ...)
  IQRna <- function(x0, ...) IQR(x0, na.rm = T, ...)
  Min   <- function(x0, ...) min(x0, na.rm = T, ...)
  Max   <- function(x0, ...) max(x0, na.rm = T, ...)
  # Do the thing based on summaries of choice:
  if (is.null(sum)) {
    rprint(x, dec)
  } else if(sum == 'N') {
    as.character(sum(!is.na(x)))
  } else if(sum == 'msd') {
    paste0(rprint(M(x),dec),' ± ',rprint(SD(x),dec))
  } else if (sum == 'IQR') {
    rprint(IQRna(x),dec)
  } else if(sum == 'minmax') {
    paste0(rprint(Min(x),dec),'-',rprint(Max(x),dec))
  } else if(sum == 'p') {
    ifelse(x < .001, '< .001', zerolead(x, dec))
  } else if(sum == 'ptext') {
    ifelse(x < .001, '< .001', paste0('= ',zerolead(x, dec)))
  } else if(sum == 'Nperc') {
    t <- table(x)
    paste0(t[2],' (',rprint(100*prop.table(t)[2], dec),'%)')
  } else if(sum == 'Nslash') {
    paste(table(x), collapse = '/')
  } else if(sum == 'estCI') {
    y <- sort(x) |> rprint(dec)
    paste0(y[2],' [',paste(y[c(1,3)], collapse = ', '),']')
  } else {
    sapply(sum, function(f) do.call(f, list(x))) |> rprint(dec)
  }
}
