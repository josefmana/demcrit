#' Fit Reference Model for Projective Prediction
#'
#' Using a regularised horseshoe prior, fits a full reference model of selected
#' "gold standard" PDD diagnosis using specified predictor set.
#'
#' @param d Data such as those calculated by \code{diagnose_pdd_sample}.
#' @param gs Name the "gold standard" variable has in `d$type`.
#' @param y Name of the PDD diagnosis variable in `d`
#' @param X Character vector containing all predictor variables from `d`.
#' @param expect_nonzero Number of variables expected to be truly predictive.
#' @param ... Other parameters for [brms::brm()].
#'
#' @details ...
#'
#' @returns List with three components:
#' \describe{
#'   \item{model}{brmsfit with the fitted model}
#'   \item{data}{Tibble with raw input data}
#'   \item{scaling}{Tibble with predictors' mean and SDs}
#' }
#'
#' @seealso
#' * [brms::horseshoe()] explains the prior implementation.
#'
#' @export
fit_reference <- function(
    d,
    gs = "Lvl.II (1)",
    y = "PDD",
    X = c(
      "moca_7",
      "moca_cloc",
      "vf_k",
      "moca_5words",
      "moca_cube",
      "moca_abs",
      "moca_anim",
      "mmse_7",
      "cloc",
      "vf_s",
      "mmse_3words",
      "mmse_pent"
    ),
    expect_nonzero = 4,
    ...
) {

  v <- c(y, X)
  miss <- v[!v %in% colnames(d)]
  if (!rlang::is_empty(miss)) {
    cli::cli_abort(c(
            "All variables in {.var y} and {.var X} must be in data.",
      "i" = "Following variable{?s} {?is/are} missing from data: {miss}"
    ))
  }

  if (!gs %in% d$type) {
    opts <- unique(d$type)
    cli::cli_abort(c(
            "{.var gs} must appear in {.code d$type}.",
      "i" = "`d$type` contains the following options, choose wisely: {opts}"
    ))
  }

  df <- d |>
    dplyr::filter(type == gs) |>
    dplyr::select(tidyselect::all_of(c(y, X))) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::all_of(y),
        .fns = as.numeric,
        .names = "y"
      ),
      dplyr::across(
        .cols = tidyselect::all_of(X),
        .fns = \(x) (x - mean(x)) / sd(x),
        .names = "{.col}_scaled"
      )
    )

  scls <- purrr::map_dfr(X, function(x) {
    tibble::tibble(
      x = x,
      M = mean(df[[x]]),
      SD = sd(df[[x]])
    )
  })

  form <- brms::bf(glue::glue("y ~ {paste(paste0(X, '_scaled'), collapse = ' + ')}"))

  rat <- expect_nonzero / length(X)
  prior <- brms::set_prior(
    paste0("horseshoe(df = 3, par_ratio = ", rat, ")"),
    class = "b"
  )

  fit <- brms::brm(
    formula = form,
    data = df,
    family = brms::bernoulli(link = "logit"),
    prior = prior,
    ...
  )

  list(
    model = fit,
    data = df,
    scaling = scls
  )
}
