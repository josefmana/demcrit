#' Grid posterior predictions of a model
#'
#' Given a grid of predictor values, their scaling, and a Bayesian logistic
#' regression model, prints a prediction grid heatmap.
#'
#' @param mod brms or projpred model
#' @param pgrid A grid of predictor values.
#' @param scls A scaling table with columns "x" for predictor name, "M" for mean
#'   and "SD" for standard deviation.
#' @param perc Percentage value for computation of the equal-tailed posterior
#'   interval \(ETI\).
#' @param lt Logit threshold(s) for declaring PDD.
#' @param cols Text colours for separating PDD classification according to
#'   thresholds in `lt`. Must be of length `length(lt) + 1`
#' @param pal Colour pallete pushed to [ggplot2::scale_fill_viridis_c()].
#' @param dir Which way should be `pal` scaled? Pushed to
#'   [ggplot2::scale_fill_viridis_c()] as `direction`.
#'
#' @returns \{ggplot2\} heatmap with estimates and `perc`% ETIs.
#'
#' @export
grid_postpred <- function(
    mod,
    pgrid = NULL,
    scls = NULL,
    perc = .95,
    lt = c(-1.293350, -2.649892),
    cols = c("grey80", "black", "red4"),
    pal = "viridis",
    dir = 1
) {

  if (length(cols) != (length(lt) + 1)) {
    cli::cli_abort(c(
            "Length of {.var cols} must equal `length(lt) + 1`.",
      "i" = "Lenght of {.var cols} is {length(cols)}.",
      "i" = "Lenght of {.var lt} is {length(lt)}."
    ))
  }

  grid <- pgrid %||% tidyr::expand_grid(
    moca_5words = 0:5,
    mmse_7 = 0:5
  )

  scales <- scls %||% tibble::tibble(
    x = c("moca_5words", "mmse_7"),
    M = c(1.9736842, 4.4368421),
    SD = c(1.8124619, 0.9670281)
  )

  b <- posterior::as_draws_rvars(mod)
  X <- scales$x

  grid <- grid |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::all_of(X),
      .fns = function(i) {
        scl <- scales |>
          dplyr::filter(x == dplyr::cur_column())
        M  <- scl |> dplyr::pull(M)
        SD <- scl |> dplyr::pull(SD)

        (i - M) / SD
      },
      .names = "{.col}_scaled"
    ))

  grid$linpred <- b$b_Intercept
  for (v in X) {
    grid$linpred <- grid$linpred +
      b[[paste0("b_", v, "_scaled")]] * grid[[paste0(v, "_scaled")]]
  }

  grid$linpred_mean <- mean(grid$linpred)
  grid$prob_PDD <- 1 / (1 + exp(-grid$linpred))
  grid$prob_mean <- mean(grid$prob_PDD)
  grid$prob_lo <- posterior::quantile2(grid$prob_PDD, (1 - perc) / 2)
  grid$prob_hi <- posterior::quantile2(grid$prob_PDD, 1 - ((1 - perc) / 2))

  #thr <- plogis(lt)
  grid$prob_class <- cut(
    grid$linpred_mean,
    breaks = c(-Inf, lt, Inf),
    labels = seq_along(cols),
    right = TRUE
  )

  grid$label <- sprintf(
    "%.2f\n[%.2f, %.2f]",
    grid$prob_mean,
    grid$prob_lo,
    grid$prob_hi
  )

  grid |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data[[X[1]]],
      y = .data[[X[2]]],
      fill = prob_mean
    ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(label = label, colour = prob_class),
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_fill_viridis_c(name = "Pr(PDD)", option = pal, direction = dir) +
    ggplot2::theme_bw()
}
