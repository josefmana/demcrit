#' Orders and prints algorithms based on their estimated PDD rates
#'
#' This function computes summaries of the estimated PDD rates,
#' stratified by diagnostic operationalisation.
#'
#' @param d0 A list with PDD data generated via \code{diagnose_pdd_sample}.
#' @param vars A data.frame, tibble, or matrix with variable names in the first column,
#' variable labels in the second column, type of variable (continuous, binary, or nominal)
#' in the third column, and optionally a fourth column denoting group and a fifth column
#' mapping each label to its description in the table’s note.
#' Alternatively, a path to a CSV file (semicolon-delimited) containing such a table.
#' @param descending A logical indicating whether operationalisations with the highest
#' estimated PDD rates should be listed first (default is \code{TRUE}).
#'
#' @returns A list with:
#' \describe{
#'   \item{\code{table}}{A tibble containing raw summaries of PDD rates.}
#'   \item{\code{plot}}{A \code{ggplot2} object visualising estimated PDD rates.}
#'   \item{\code{gtable}}{A named list of gt tables:
#'      \describe{
#'        \item{\code{gtab_rates}{An APA-style \code{gt} table summarising the results.}}
#'        \item{\code{gtab_algos}{An APA-style \code{gt} table listing algorithms used.}}
#'        }}
#' }
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
    v <- readr::read_delim(vars, delim = ";", col_types = cols())
  } else {
    v <- vars
  }
  # Prepare and arrange the table with prevalences:
  prevs <-
    d0$PDD |>
    select(type, PDD) |>
    table() |>
    as_tibble() |>
    pivot_wider(names_from = PDD, values_from = n) |>
    mutate(
      N = rowSums(across(all_of(c("FALSE", "TRUE")))),
      perc = 100 * `TRUE`/N,
      Rate = paste0(`TRUE`, " (", do_summary(perc, 2), "%)")
    )
  # Prepare operationalisation labels:
  opers <-
    d0$criteria |>
    mutate(
      Global = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == glob[i], 2]," < ", glob_t[i])),
      Attention = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == atte[i], 2]," < ", atte_t[i])),
      Executive = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == exec[i], 2]," < ", exec_t[i])),
      Construction = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == cons[i], 2]," < ", cons_t[i])),
      Memory = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == memo[i], 2]," < ", memo_t[i])),
      Language = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == lang[i], 2]," < ", lang_t[i])),
      IADL = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == iadl[i], 2]," > ", iadl_t[i]))
    ) |>
    select(type, Global, Attention, Executive, Construction, Memory, Language, IADL) |>
    mutate_all(~ifelse(grepl("NA",.x), "-", .x))
  # Make the table:
  tab <-
    prevs |>
    left_join(opers, by = "type") |>
    select(-`FALSE`, -`TRUE`) |>
    relocate(N, .after = last_col()) |>
    relocate(Rate, .after = last_col()) |>
    mutate(
      Global = if_else(grepl("Lvl.II", type), "-", Global),
      Attention = if_else(grepl("Lvl.II", type), "TMT A < -1.5 OR WAIS DS < -1.5", Attention),
      Executive = if_else(grepl("Lvl.II", type), "CF A < -1.5 OR PST C < -1.5", Executive),
      Construction = if_else(grepl("Lvl.II", type), "JoLO < -1.5 OR CLOXI < -1.5", Construction),
      Memory = if_else(grepl("Lvl.II", type), "RAVLT-DR < -1.5 OR BVMTR-DR < -1.5 OR WMS-III Family Pictures < -1.5", Memory),
      Language = if_else(grepl("Lvl.II", type), "WAIS Similarities < -1.5 OR BNT 60 < -1.5", Language)
    )
  # Make a table showing estimated PDD rates:
  gtab_rates <-
    tab |>
    arrange(desc(perc)) |>
    select(type, N, Rate) |>
    gt_apa_table() |>
    cols_label(type ~ "Algorithm")
  # Make a table showing algorithms' specification:
  gtab_algos <-
    tab |>
    select(-perc, -N, -Rate) |>
    gt_apa_table() |>
    tab_spanner(
      columns = c("Attention", "Executive", "Construction", "Memory", "Language"),
      label = "Impaired cognition",
      gather = FALSE
    ) |>
    cols_label(
      type ~ "Algorithm",
      Global ~ "Global deficit",
      Executive ~ "Executive function",
      IADL ~ "Impact on IADLs"
    )
  # If there are notes, add them to the table:
  if (ncol(v) > 4 && any(!is.na(v$note))) {
    notes <- v[complete.cases(v[ , 5]), c(2,5)]
    text <- paste0(pull(notes[ , 1]),": ",pull(notes[ , 2])) |> paste(collapse = ", ")
    # Add the text:
    gtab_algos <-
      gtab_algos |>
      tab_source_note(html(paste0("<i>Note.</i> ",text)))
  }
  # Visualisation code:
  smoca_9 <- subset(prevs, type == subset(d0$criteria, group == "smoca" & iadl == "faq_9")$type)$perc
  smoca_tot <- subset(prevs, type == subset(d0$criteria, group == "smoca" & iadl == "faq"  )$type)$perc
  lvlII_9 <- subset(prevs, type == subset(d0$criteria, group == "lvlII" & iadl == "faq_9")$type)$perc
  lvlII_tot <- subset(prevs, type == subset(d0$criteria, group == "lvlII" & iadl == "faq"  )$type)$perc
  plt <-
    prevs |>
    filter(!grepl("smoca", type)) |>
    mutate(
      kind = sapply(
        seq_len(length(type)),
        function(i) case_when(
          d0$criteria$glob[d0$criteria$type == type[i]] == "mmse" & d0$criteria$iadl[d0$criteria$type == type[i]] == "faq_9" ~ 1,
          d0$criteria$glob[d0$criteria$type == type[i]] == "mmse" & d0$criteria$iadl[d0$criteria$type == type[i]] == "faq" ~ 2,
          d0$criteria$glob[d0$criteria$type == type[i]] == "moca_total" & d0$criteria$iadl[d0$criteria$type == type[i]] == "faq_9" ~ 3,
          d0$criteria$glob[d0$criteria$type == type[i]] == "moca_total" & d0$criteria$iadl[d0$criteria$type == type[i]] == "faq" ~ 4
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
    ggplot() +
    aes(x = perc, colour = `Operationalized by:`, fill = `Operationalized by:`) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30) +
    geom_density(lwd = 1, alpha = .25) +
    geom_vline(xintercept = smoca_9, lwd = 1, lty = "dotted", colour = "orange3") +
    geom_vline(xintercept = smoca_tot, lwd = 1, lty = "dotted", colour = "blue") +
    geom_vline(xintercept = lvlII_9, lwd = 1, lty = "dashed", colour = "orange3") +
    geom_vline(xintercept = lvlII_tot, lwd = 1, lty = "dashed", colour = "blue") +
    #scale_colour_manual(values = c("brown", "purple2", "orange", "violet")) +
    #scale_fill_manual(values = c("brown", "purple2", "orange", "violet")) +
    labs(x = "Estimated PDD rate (%)", y = "Density") +
    theme(legend.position = "bottom")
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
