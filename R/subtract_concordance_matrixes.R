#' Compare concordance matrixes
#'
#' Given two sets of \code{describe_concordance} results, computes difference
#' matrixes for Cohen's Kappa, Accuracy, Sensitivity, and Specificity.
#'
#' @param conc0 The subtrahend concordance statistics
#' @param conc1 The minuend concordance statistics
#' @param label0 How should the subtrahend be called?
#' @param label1 How should the minuend be called?
#'
#' @returns A list with two components:
#' \describe{
#'   \item{\code{differences}}{Tibble containing rowwise differences}.
#'   \item{\code{plots}}{A list of ggplot2-based visualisations of differences}
#' }
#'
#' @export
subtract_concordance_matrixes <- function(
    conc0,
    conc1,
    label0 = "0",
    label1 = "1"
) {

  nms <- c("Cohen's \u03ba", "Accuracy", "Sensitivity", "Specificity")
  labs <- purrr::map_chr(nms, function(x) {
    glue::glue("{x}\n({label1} - {label0})")
  })

  vars <- c("Kappa_raw", "Accuracy_raw", "Sensitivity", "Specificity")
  scaling <- purrr::map_dbl(vars, function(x) {
    sd0 <- sd(conc0[[x]], na.rm = TRUE)
    sd1 <- sd(conc1[[x]], na.rm = TRUE)
    mean(sd0, sd1)
  })

  diffmat <- conc0 |>
    dplyr::select(predictor, reference) |>
    dplyr::bind_cols(conc1[, vars] - conc0[, vars])

  loopval <- rlang::set_names(
    x = seq_along(vars),
    nm = stringr::str_remove(vars, "_raw")
  )

  plots <- purrr::map(loopval, function(i) {
    diffmat |>
      ggplot2::ggplot() +
      ggplot2::aes(x = predictor, y = reference, fill = .data[[vars[i]]]) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(
        high = "red",
        low = "blue",
        na.value = "grey95",
        limits = c(-scaling[i], scaling[i])
      ) +
      ggplot2::labs(x = "Predictor", y = "Reference", fill = labs[i]) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 66, hjust = 1))
  })

  list(
    differences = diffmat,
    plots = plots
  )
}
