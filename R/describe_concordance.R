#' Computes and visualises measures of concordance
#' between pairs of criteria.
#'
#' This function computes pairwise statistics of
#' probable PDD operationalisations concordance.
#' The statistics include Cohen's Kappa, Sensitivity/PPV,
#' Specificity/NPV and Accuracy.
#'
#' @param d0 A list with PDD data generated via
#' \code{diagnose_pdd_sample}.
#' @param vars A data.frame/tibble/matrix with variable
#' names in the first column, variable labels in the
#' second column, type of variable (continuous, binary or
#' nominal) in the third column, and optionally a fourth column
#' denoting group and a fifth column that maps each
#' label to its description in the table's note.
#' Alternatively, a path to a csv delimited by semi-colon
#' containing such a table.
#'
#' @returns
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#' pdd  <- diagnose_pdd_sample(data)
#' vars <- here::here('data-raw','VariablesOfInterest.csv')
#'
#' concmats <- describe_concordance(data, vars)
#' }
#' @export
describe_concordance <- function(d0, vars) {
  # Get pairs of operationalisations:
  p <- crossing(crit1 = d0$criteria$type, crit2 = d0$criteria$type)
  # Calculate Kappas:
  k <- lapply(
    seq_len(nrow(p)),
    function(i) {
      if(p[i, 1] == p[i, 2]) {
        list(kappa = 1)
      } else {
        d0$PDD |>
          filter(type %in% p[i, ]) |>
          pivot_wider(names_from = type, values_from = PDD, id_cols = id) |>
          select(-id) |>
          as.matrix() |>
          cohen.kappa()
      }
    }
  )
  # Calculate confusion matrixes:
  confmats <- lapply(
    seq_len(nrow(p)),
    function(i) {
      if(p[i, 1] == p[i, 2]) {
        NULL
      } else {
        d0$PDD |>
          filter(type %in% p[i, ]) |>
          pivot_wider(names_from = type, values_from = PDD, id_cols = id) |>
          select(-id) |>
          mutate_all(~2-as.numeric(.x)) |> # re-scoring such that prevalence is calculated for PDD == 1
          table() |>
          confusionMatrix()
      }
    }
  )
  # Prepare a table with all statistics:
  tab <-
    p |>
    mutate(
      Kappa = sapply(
        seq_len(length(crit1)),
        function(i) ifelse(
          test = crit1[i] == crit2[i],
          yes  = '-',
          no   = do_summary(k[[i]]$confid[1, ], 2, 'estCI')
        )
      )
    )
}
