#' Summarise data using predefined summary types
#'
#' Computes descriptive summaries of a given vector, optionally including graphical
#' representation (planned for future versions). Supports various summary types
#' tailored for continuous, binary, and nominal variables.
#'
#' @param x A vector of data to be summarised.
#' @param dec An integer specifying the number of decimal places for numeric summaries.
#' @param sum A character string specifying the type of summary to compute. Possible values:
#' \describe{
#'   \item{NULL (default)}{Simple printing of the vector}
#'   \item{"N"}{Number of observations}
#'   \item{"msd"}{Mean ± standard deviation}
#'   \item{"M"}{Mean}
#'   \item{"SD"}{Standard deviation}
#'   \item{"Md"}{Median}
#'   \item{"IQR"}{Interquartile range}
#'   \item{"minmax"}{Minimum–maximum range}
#'   \item{"Min"}{Minimum}
#'   \item{"Max"}{Maximum}
#'   \item{"p"}{p-value (formatted for tables)}
#'   \item{"ptext"}{p-value (formatted for in-text reporting)}
#'   \item{"Nperc"}{Count and percentage for binary variables}
#'   \item{"Nslash"}{Counts separated by slashes for nominal variables with any number of categories}
#'   \item{"estCI"}{Estimate with confidence interval, e.g., "estimate [CI]"}
#' }
#'
#' @returns A character string or numeric summary as specified by \code{sum}.
#'
#' @examples
#' \dontrun{
#' p <- data_paths("data-raw")
#' data <- prepare_data(p)
#'
#' M <- do_summary(data[, 25], 2, "M")
#' SD <- do_summary(data[, 25], 2, "SD")
#' Md <- do_summary(data[, 25], 2, "Md")
#' mm <- do_summary(data[, 25], 0, "minmax")
#' }
#'
#' @export
do_summary <- function(x, dec, sum = NULL) {
  # Prepare a functions for printing:
  rprint <- function(x0, dec0) {
    sprintf(paste0('%.',dec0,'f'), round(x0, dec0))
  }
  zerolead <- function(x1, dec1) {
    sub('0.', '.', rprint(x1, dec1), fixed = TRUE)
  }
  # Prepare functions for extracting statistics ignoring NAs:
  M     <- function(x0, ...) mean(x0, na.rm = TRUE, ...)
  Md    <- function(x0, ...) median(x0, na.rm = TRUE, ...)
  SD    <- function(x0, ...) sd(x0, na.rm = TRUE, ...)
  IQRna <- function(x0, ...) IQR(x0, na.rm = TRUE, ...)
  Min   <- function(x0, ...) min(x0, na.rm = TRUE, ...)
  Max   <- function(x0, ...) max(x0, na.rm = TRUE, ...)
  # Do the thing based on summaries of choice:
  if (is.null(sum)) {
    rprint(x, dec)
  } else if(sum == "N") {
    as.character(sum(!is.na(x)))
  } else if(sum == "msd") {
    paste0(rprint(M(x),dec)," ± ",rprint(SD(x), dec))
  } else if (sum == "IQR") {
    rprint(IQRna(x),dec)
  } else if(sum == "minmax") {
    paste0(rprint(Min(x), dec),"-",rprint(Max(x), dec))
  } else if(sum == "p") {
    ifelse(x < .001, "< .001", zerolead(x, dec))
  } else if(sum == "ptext") {
    ifelse(x < .001, "< .001", paste0("= ", zerolead(x, dec)))
  } else if(sum == "Nperc") {
    t <- table(x)
    paste0(t[2]," (",rprint(100*prop.table(t)[2], dec), "%)")
  } else if(sum == "Nslash") {
    paste(table(x), collapse = "/")
  } else if(sum == "estCI") {
    y <- sort(x) |> rprint(dec)
    paste0(y[2], " [", paste(y[c(1,3)], collapse = ", "), "]")
  } else {
    sapply(sum, function(f) do.call(f, list(x))) |> rprint(dec)
  }
}
