#' Calculates z-score(s) based on raw score, regression
#' calculator values and demographic variables.
#'
#' This function takes in regression calculator, raw
#' performance metrics, age, gender and education level
#' of patients and computes a set of z-scores out of it.
#'
#' @param calc A data.frame/tibble with regression parameters
#' of the calculator.
#' @param x A vector of raw performance scores.
#' @param nam A character containing test index label.
#' @param AGE A numeric denoting years of age.
#' @param GEN A numeric denoting gender (Man = 1,
#' Woman = 0)
#' @param EDU A numeric denoting years of education.
#'
#' @returns
#'
#' @examples
#' \dontrun{
#' }
#' @export
compute_z_score <- function(calc, x, nam, AGE, GEN, EDU) {
  with(
    calc, {
      pars <- as.numeric(c(Constant[calc_lab == nam], age[calc_lab == nam], gender[calc_lab == nam], education[calc_lab == nam]))
      data <- as.matrix(cbind(rep(1, length(x)), AGE, GEN, EDU))
      x_bar <- data %*% pars
      z <- c(sign[calc_lab == nam] * (x - x_bar) / RMSE[calc_lab == nam])
      z
    }
  )
}
