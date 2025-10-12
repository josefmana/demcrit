#' Compute Regression z-scores
#'
#' Computes z-scores for a given test index using regression parameters from a normative
#' calculator, along with the patient's raw score and demographic information
#' (age, gender, education).
#'
#' @param calc A data frame or tibble containing regression parameters from the calculator.
#' @param x A numeric vector of raw performance scores.
#' @param lbl A character string specifying the test index label (must match an entry in \code{calc}).
#' @param AGE A numeric vector indicating the participant's age (in years).
#' @param GEN A numeric vector indicating gender (1 = man, 0 = woman).
#' @param EDU A numeric vector indicating years of education.
#'
#' @returns A numeric vector of computed z-scores, one for each raw score in \code{x}.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' z <- compute_z_score(
#'   calc = regression_table,
#'   x = c(35, 42, 28),
#'   lbl = "Digit Span Backward",
#'   AGE = c(67, 70, 74),
#'   GEN = c(1, 0, 1),
#'   EDU = c(15, 12, 16)
#' )
#' }
#' @export
compute_z_score <- function(calc, x, lbl, AGE, GEN, EDU) {
  with(calc, {
    pars <- as.numeric(c(Constant[calc_lab == lbl], age[calc_lab == lbl], gender[calc_lab == lbl], education[calc_lab == lbl]))
    data <- as.matrix(cbind(rep(1, length(x)), AGE, GEN, EDU))
    x_bar <- data %*% pars
    z <- c(sign[calc_lab == lbl] * (x - x_bar) / RMSE[calc_lab == lbl])
    z
  })
}
