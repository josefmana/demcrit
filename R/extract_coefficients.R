#' Extract point estimate of model coefficients
#'
#' Given posterior draws from a {brms} model or {projpred} projection and
#' scaling values for predictors, converts coefficients from scaled predictors
#' to the original variable scale and returns summary statistics.
#'
#' @param mod Model object or posterior draws compatible with
#'   `posterior::as_draws_matrix()`.
#' @param scls Scaling values as exported by \code{fit_reference}.
#' @param stat Summary statistic applied to posterior draws.
#'
#' @returns Named numeric vector of coefficients on the original scale.
#
#' @export
extract_coefficients <- function(mod, scls, stat = "mean") {

  drws <- posterior::as_draws_matrix(mod)

  pars <- colnames(drws) |>
    stringr::str_remove("^b_") |>
    stringr::str_remove("_scaled$")
  colnames(drws) <- pars

  scales <- scls |>
    dplyr::filter(x %in% pars) |>
    tibble::column_to_rownames("x")
  scales <- scales[pars[-1], , drop = FALSE] # Enforce correct order

  b0 <- drws[, 1]
  b <- drws[, -1, drop = FALSE]

  rat <- scales$M / scales$SD

  intercept_raw <- b0 - as.vector(b %*% rat)
  beta_raw <- sweep(b, 2, scales$SD, "/")
  coefs_raw <- cbind(Intercept = intercept_raw, beta_raw)

  apply(coefs_raw, 2, match.fun(stat))
}
