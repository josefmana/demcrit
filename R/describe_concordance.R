#' Compute and visualise measures of concordance between diagnostic criteria
#'
#' Computes pairwise concordance statistics between different operationalisations
#' of probable Parkinson’s disease dementia (PDD). Metrics include Cohen's Kappa,
#' Sensitivity, Positive Predictive Value (PPV), Specificity, Negative Predictive
#' Value (NPV), and Accuracy. Visual summaries are also generated.
#'
#' @param d0 A list containing PDD diagnostic results, as produced by
#'    \code{diagnose_pdd_sample}.
#'
#' @returns A list with two components:
#' \describe{
#'   \item{\code{table}}{A data frame summarising pairwise concordance statistics
#'   (e.g., Kappa, Accuracy, Sensitivity, Specificity, PPV, NPV).}
#'   \item{\code{plots}}{A list of ggplot2-based visualisations, including matrices
#'   of Cohen's Kappa, Accuracy (with significance tests against the No Information Rate),
#'   Sensitivity, and Specificity.}
#' }
#'
#' @examples
#' \dontrun{
#' p <- data_paths("data-raw")
#' data <- prepare_data(p)
#' pdd <- diagnose_pdd_sample(data)
#' concmats <- describe_concordance(pdd)
#' }
#' @export
describe_concordance <- function(d0) {
  # Get pairs of operationalisations:
  p <- tidyr::crossing(predictor = d0$criteria$type, reference = d0$criteria$type)
  # Calculate Kappas:
  k <- lapply(seq_len(nrow(p)), function(i) {
    if (p[i, 1] == p[i, 2]) {
      list(kappa = 1)
    } else {
      d0$PDD |> dplyr::filter(type %in% p[i, ]) |>
        tidyr::pivot_wider(names_from = type, values_from = PDD, id_cols = id) |>
        dplyr::select(-id) |>
        as.matrix() |>
        psych::cohen.kappa()
    }
  })
  # Calculate confusion matrixes:
  confmats <- lapply(seq_len(nrow(p)), function(i) {
    if (p[i, 1] == p[i, 2]) {
      NULL
    } else {
      d0$PDD |> dplyr::filter(type %in% p[i, ]) |>
        tidyr::pivot_wider(names_from = type, values_from = PDD, id_cols = id) |>
        dplyr::select(tidyselect::all_of(unlist(p[i, ], use.names = FALSE))) |>
        mutate_all(\(x) 2 - as.numeric(x)) |> # re-scoring such that prevalence is calculated for PDD == 1
        table() |>
        caret::confusionMatrix()
    }
  })
  metrics <- names(confmats[[2]]$byClass)
  # Prepare a table with all statistics:
  # (check https://changjunlee.com/blogs/posts/4_confusion_mat_and_roc#interpreting-the-confusion-matrix
  # for a guide to interpretation)
  tab <- p |>
    mutate(
      # Estimate [95% CI] for Kappa & Accuracy:
      Kappa = sapply(seq_along(reference), function(i) {
        ifelse(
          test = reference[i] == predictor[i],
          yes = "-",
          no = do_summary(k[[i]]$confid[1, ], 2, "estCI")
        )
      }),
      Accuracy = sapply(seq_along(reference), function(i) {
        ifelse(
          test = reference[i] == predictor[i],
          yes = "1.00",
          no = do_summary(confmats[[i]]$overall[c("Accuracy", "AccuracyUpper", "AccuracyLower")], 2, "estCI")
        )
      }),
      # Raw values for Kappa & Accuracy (and NIR):
      Kappa_raw = sapply(seq_along(reference), function(i) {
        ifelse(
          test = reference[i] == predictor[i],
          yes = NA,
          no = k[[i]]$kappa
        )
      }),
      McnemarPValue = sapply(seq_along(reference), function(i) {
        ifelse(
          test = reference[i] == predictor[i],
          yes = NA,
          no = confmats[[i]]$overall["McnemarPValue"]
        )
      }),
      Accuracy_raw = sapply(seq_along(reference), function(i) {
        ifelse(
          test = reference[i] == predictor[i],
          yes = 1,
          no = confmats[[i]]$overall["Accuracy"]
        )
      }),
      NoInformationRate_raw = sapply(seq_along(reference), function(i) {
        ifelse(
          test = reference[i] == predictor[i],
          yes = NA,
          no = confmats[[i]]$overall["AccuracyNull"]
        )
      }),
      AccuracyPValue = sapply(seq_along(reference), function(i) {
        ifelse(
          test = reference[i] == predictor[i],
          yes = NA,
          no = confmats[[i]]$overall["AccuracyPValue"]
        )
      }),
      # All the other metrics:
      !!!rlang::set_names(rep(NA, length(metrics)), metrics),
      dplyr::across(
        .cols = tidyselect::all_of(metrics),
        .fns = function(x) {
          sapply(seq_along(x), function(i) {
            ifelse(
              test = reference[i] == predictor[i],
              yes = NA,
              no = confmats[[i]]$byClass[dplyr::cur_column()]
            )
          })
        }
      ),
      NULL
    )
  # Get order of the criteria by prevalence:
  ord <- data.frame(
    predictor = tab |>
      dplyr::arrange(desc(Prevalence)) |>
      dplyr::distinct(reference) |>
      dplyr::pull()
  ) |>
    dplyr::mutate(
      reference = rev(predictor),
      # Colours to separate operationalisations based on IADL criterion
      xcol = sapply(seq_along(predictor), function(i) {
        ifelse(
          test = subset(d0$criteria, type == predictor[i])$iadl == "faq",
          yes = "blue2",
          no = "black"
        )
      }),
      ycol = rev(xcol)
    )
  # Add order to the table:
  ordtab <- tab |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::all_of(c("predictor", "reference")),
        .fns = \(x) factor(x, levels = ord[ , cur_column()], ordered = TRUE)
      ),
      dplyr::across(
        .cols = tidyselect::all_of(c("Kappa_raw")),
        .fns  = function(x) case_when(
          as.numeric(predictor) < (nrow(ord) + 1 - as.numeric(reference)) ~ x,
          predictor == reference ~ 1,
          .default = NA
        )
      ),
      Accuracy_raw = dplyr::if_else(predictor == reference, NA, Accuracy_raw),
      Accuracy_sig = dplyr::if_else(AccuracyPValue < .05, "*", "")
    )
  # Kappa matrix:
  kappaplt <- ordtab |>
    ggplot2::ggplot() +
    ggplot2::aes(x = predictor, y = reference, fill = Kappa_raw) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(high = "steelblue", na.value = "white") +
    ggplot2::labs(x = "Predictor", y = "Reference") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = dplyr::if_else(ord$xcol == "black", "black", "red3"), angle = 66, hjust = 1),
      axis.text.y = ggplot2::element_text(colour = dplyr::if_else(ord$ycol == "black", "black", "red3"))
    )
  # Accuracy matrix:
  E_NIR <- tab$NoInformationRate_raw |>
    unique() |>
    mean(na.rm = TRUE) # Expected Negative Information Rate
  accplt <- ordtab |>
    dplyr::select(-Accuracy) |>
    dplyr::rename("Accuracy" = "Accuracy_raw") |>
    ggplot2::ggplot() +
    ggplot2::aes(x = predictor, y = reference, fill = Accuracy) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(aes(label = Accuracy_sig)) +
    ggplot2::scale_fill_gradient2(low = "red4", high = "yellow4", midpoint = E_NIR, na.value = "white") +
    ggplot2::labs(x = "Predictor", y = "Reference") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = ord$xcol, angle = 66, hjust = 1),
      axis.text.y = ggplot2::element_text(colour = ord$ycol)
    )
  # Sensitivity matrix:
  sensplt <- ordtab |>
    ggplot2::ggplot() +
    ggplot2::aes(x = predictor, y = reference, fill = Sensitivity) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(high = "green4", na.value = "white", midpoint = .5) +
    ggplot2::labs(x = "Predictor", y = "Reference") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = ord$xcol, angle = 66, hjust = 1),
      axis.text.y = ggplot2::element_text(colour = ord$ycol)
  )
  # Specificity matrix:
  specplt <- ordtab |>
    ggplot2::ggplot() +
    ggplot2::aes(x = predictor, y = reference, fill = Specificity) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(high = "orange3", na.value = "white", midpoint = .5) +
    ggplot2::labs(x = "Predictor", y = "Reference") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = ord$xcol, angle = 66, hjust = 1),
      axis.text.y = ggplot2::element_text(colour = ord$ycol)
    )
  # Return it all:
  list(
    table = tab,
    plots = list(
      Kappa = kappaplt,
      Accuracy = accplt,
      Sensitivity = sensplt,
      Specificity = specplt
    )
  )
}
