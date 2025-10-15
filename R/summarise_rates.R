#' Order and Prints Algorithms
#'
#' This function computes summaries of the estimated PDD rates,
#' stratified by diagnostic algorithm.
#'
#' @param d0 A list with PDD data generated via \code{diagnose_pdd_sample}.
#' @param vars A data.frame, tibble, or matrix with  in the
#'    following order:
#'
#'    1. variable names,
#'    2. variable labels
#'    3. type of variable (continuous, binary, or nominal)
#'    4. optional, group,
#'    5. optional, mapping each label to its description in the table’s note.
#'
#'    Alternatively, a path to a CSV file (semicolon-delimited) containing
#'    such a table.
#' @param descending A logical indicating whether algorithms with the highest
#'    estimated PDD rates should be listed first (default is `TRUE`).
#'
#' @returns A list with:
#' \describe{
#'   \item{\code{table}}{A tibble containing raw summaries of PDD rates.}
#'   \item{\code{plot}}{A \code{ggplot2} object visualising estimated PDD rates.}
#'   \item{\code{gtable}}{A named list of gt tables:
#'      \describe{
#'        \item{\code{gtab_rates}}{An APA-style \code{gt} table summarising the results.}
#'        \item{\code{gtab_algos}}{An APA-style \code{gt} table listing algorithms used.}
#'        }}
#' }
#'
#' @seealso [diagnose_pdd_sample()] prepares `d0`.
#'
#' @examples
#' \dontrun{
#' p <- data_paths("data-raw")
#' data <- prepare_data(p)
#' pdd  <- diagnose_pdd_sample(data)
#' vars <- here::here("data-raw", "VariablesOfInterest.csv")
#'
#' rates0 <- summarise_rates(pdd, vars)
#' rates1 <- summarise_rates(pdd, vars, TRUE)  # rates0 and rates1 are identical
#' rates2 <- summarise_rates(pdd, vars, FALSE) # ascending order
#' }
#'
#' @export
summarise_rates <- function(d0, vars, descending = TRUE) {
  # Get variables mapping:
  if (is.character(vars)) {
    v <- readr::read_delim(vars, delim = ";", col_types = readr::cols())
  } else {
    v <- vars
  }
  # Prepare and arrange the table with PDD rates (previously called 'prevalences'):
  prevs <- d0$PDD |>
    dplyr::select(type, PDD) |>
    table() |>
    tibble::as_tibble() |>
    tidyr::pivot_wider(names_from = PDD, values_from = n) |>
    dplyr::mutate(
      N = rowSums(dplyr::across(tidyselect::all_of(c("FALSE", "TRUE")))),
      perc = 100 * `TRUE` / N,
      Rate = paste0(`TRUE`, " (", do_summary(perc, 2), "%)")
    )
  # Prepare algorithm (previously called 'operationalisation') labels:
  opers <- d0$algorithms |>
    dplyr::mutate(
      Global = sapply(seq_along(type), \(i) paste0(v[v[ , 1] == glob[i], 2]," < ", glob_t[i])),
      Attention = sapply(seq_along(type), \(i) paste0(v[v[ , 1] == atte[i], 2]," < ", atte_t[i])),
      Executive = sapply(seq_along(type), \(i) paste0(v[v[ , 1] == exec[i], 2]," < ", exec_t[i])),
      Construction = sapply(seq_along(type), \(i) paste0(v[v[ , 1] == cons[i], 2]," < ", cons_t[i])),
      Memory = sapply(seq_along(type), \(i) paste0(v[v[ , 1] == memo[i], 2]," < ", memo_t[i])),
      Language = sapply(seq_along(type), \(i) paste0(v[v[ , 1] == lang[i], 2]," < ", lang_t[i])),
      IADL = sapply(seq_along(type), \(i) paste0(v[v[ , 1] == iadl[i], 2]," > ", iadl_t[i]))
    ) |>
    dplyr::select(type, Global, Attention, Executive, Construction, Memory, Language, IADL) |>
    dplyr::mutate_all(\(x) ifelse(grepl("NA", x), "-", x))
  # Make the table:
  tab <- prevs |>
    dplyr::left_join(opers, by = "type") |>
    dplyr::select(-`FALSE`, -`TRUE`) |>
    dplyr::relocate(N, .after = tidyselect::last_col()) |>
    dplyr::relocate(Rate, .after = tidyselect::last_col()) |>
    dplyr::mutate(
      Global = dplyr::if_else(grepl("Lvl.II", type), "-", Global),
      Attention = dplyr::if_else(grepl("Lvl.II", type), "TMT A < -1.5 OR WAIS DS < -1.5", Attention),
      Executive = dplyr::if_else(grepl("Lvl.II", type), "CF A < -1.5 OR PST C < -1.5", Executive),
      Construction = dplyr::if_else(grepl("Lvl.II", type), "JoLO < -1.5 OR CLOXI < -1.5", Construction),
      Memory = dplyr::if_else(grepl("Lvl.II", type), "RAVLT-DR < -1.5 OR BVMTR-DR < -1.5 OR WMS-III Family Pictures < -1.5", Memory),
      Language = dplyr::if_else(grepl("Lvl.II", type), "WAIS Similarities < -1.5 OR BNT 60 < -1.5", Language)
    )
  # Make a table showing estimated PDD rates:
  gtab_rates <- tab |>
    dplyr::arrange(dplyr::desc(perc)) |>
    dplyr::select(type, N, Rate) |>
    gt_apa_table() |>
    gt::cols_label(type ~ "Algorithm") |>
    gt::tab_source_note(gt::html("<i>Note.</i> Percentages were calculated from all available cases. The items comprising each listed algorithm can be found in Table A1."))
  # Make a table showing algorithms' specification:
  gtab_algos <- tab |>
    dplyr::select(-perc, -N, -Rate) |>
    gt_apa_table() |>
    gt::tab_spanner(
      columns = c("Attention", "Executive", "Construction", "Memory", "Language"),
      label = "Impaired cognition",
      gather = FALSE
    ) |>
    gt::cols_label(
      type ~ "Algorithm",
      Global ~ "Global deficit",
      Executive ~ "Executive function",
      IADL ~ "Impact on IADLs"
    )
  # If there are notes, add them to the table:
  if (ncol(v) > 4 && any(!is.na(v$note))) {
    notes <- v[complete.cases(v[ , 5]), c(2,5)]
    text <- paste0(dplyr::pull(notes[ , 1]), ": ", dplyr::pull(notes[ , 2])) |> paste(collapse = ", ")
    # Add the text:
    gtab_algos <- gtab_algos |>
      gt::tab_source_note(gt::html(glue::glue("<i>Note.</i> {text}")))
  }
  # Visualisation code:
  smoca_9 <- subset(prevs, type == subset(d0$algorithms, group == "smoca" & iadl == "faq_9")$type)$perc
  smoca_tot <- subset(prevs, type == subset(d0$algorithms, group == "smoca" & iadl == "faq")$type)$perc
  lvlII_9 <- subset(prevs, type == subset(d0$algorithms, group == "lvlII" & iadl == "faq_9")$type)$perc
  lvlII_tot <- subset(prevs, type == subset(d0$algorithms, group == "lvlII" & iadl == "faq"  )$type)$perc
  plt <- prevs |>
    dplyr::filter(!grepl("sMoCA|Lvl.II", type)) |>
    dplyr::mutate(
      kind = sapply(
        seq_len(length(type)),
        function(i) dplyr::case_when(
          d0$algorithms$glob[d0$algorithms$type == type[i]] == "mmse" & d0$algorithms$iadl[d0$algorithms$type == type[i]] == "faq_9" ~ 1,
          d0$algorithms$glob[d0$algorithms$type == type[i]] == "mmse" & d0$algorithms$iadl[d0$algorithms$type == type[i]] == "faq" ~ 2,
          d0$algorithms$glob[d0$algorithms$type == type[i]] == "moca_total" & d0$algorithms$iadl[d0$algorithms$type == type[i]] == "faq_9" ~ 3,
          d0$algorithms$glob[d0$algorithms$type == type[i]] == "moca_total" & d0$algorithms$iadl[d0$algorithms$type == type[i]] == "faq" ~ 4
        )
      ),
      `Operationalized by:` = factor(
        x = kind,
        levels = 1:4,
        labels = c(
          "MMSE & FAQ (it. 9)",
          "MMSE & FAQ (total)",
          "MoCA & FAQ (it. 9)",
          "MoCA & FAQ (total)"
        ),
        ordered = FALSE
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = perc, colour = `Operationalized by:`, fill = `Operationalized by:`) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), colour = "black", fill = "white", bins = 30) +
    ggplot2::geom_density(lwd = 1, alpha = .25) +
    ggplot2::geom_vline(xintercept = smoca_9, lwd = 1, lty = "dotted", colour = "orange3") +
    ggplot2::geom_vline(xintercept = smoca_tot, lwd = 1, lty = "dotted", colour = "blue") +
    ggplot2::geom_vline(xintercept = lvlII_9, lwd = 1, lty = "dashed", colour = "orange3") +
    ggplot2::geom_vline(xintercept = lvlII_tot, lwd = 1, lty = "dashed", colour = "blue") +
    ggplot2::labs(x = "Estimated PDD rate (%)", y = "Density") +
    ggplot2::theme(legend.position = "bottom")
  # Return:
  list(
    table = tab,
    plot = plt,
    gtables = list(
      rates = gtab_rates,
      algorithms = gtab_algos
    )
  )
}
