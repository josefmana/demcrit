#' Orders and prints PDD operationalisations based
#' on their estimated prevalence.
#'
#' This function computes summaries of the
#' prevalence estimates stratified by PDD
#' operationalisation.
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
#' @param descending A logical indicating whether
#' operationalisations with the highest estimated
#' PDD prevalence should be on the top (by default
#' descending = T).
#'
#' @returns A list with a tibble containing
#' raw summaries, a APA-style gt table and
#' a ggplot summarising prevalence estimates.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#' pdd  <- diagnose_pdd_sample(data)
#' vars <- here::here('data-raw','VariablesOfInterest.csv')
#'
#' prevtab0 <- summarise_prevalence(pdd, vars)
#' prevtab1 <- summarise_prevalence(pdd, vars, T) # prevtab0 & prevtab1 are identical
#' prevtab2 <- summarise_prevalence(pdd, vars, F)
#' }
#' @export
summarise_prevalence <- function(d0, vars, descending = T) {
  # Get variables mapping:
  if (is.character(vars)) {
    v <- readr::read_delim(vars, delim = ';', col_types = cols())
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
      N = rowSums(across(all_of(c('FALSE', 'TRUE')))),
      perc = 100 * `TRUE`/N,
      Prevalence = paste0(`TRUE`,' (',do_summary(perc, 2),'%)')
    ) |>
    arrange(desc(perc))
  # Prepare operationalisation labels:
  opers <-
    d0$criteria |>
    mutate(
      Global       = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == glob[i] ,2],' < ', glob_t[i])),
      Attention    = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == atte[i] ,2],' < ', atte_t[i])),
      Executive    = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == exec[i] ,2],' < ', exec_t[i])),
      Construction = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == cons[i] ,2],' < ', cons_t[i])),
      Memory       = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == memo[i] ,2],' < ', memo_t[i])),
      Language     = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == lang[i] ,2],' < ', lang_t[i])),
      IADL         = sapply(seq_len(length(type)), function(i) paste0(v[v[ , 1] == iadl[i] ,2],' > ', iadl_t[i]))
    ) |>
    select(type, Global, Attention, Executive, Construction, Memory, Language, IADL) |>
    mutate_all(~ifelse(grepl('NA',.x), '-', .x))
  # Make the table:
  tab <-
    prevs |>
    left_join(opers, by = 'type') |>
    select(-type, -`FALSE`, -`TRUE`) |>
    relocate(N, .after = last_col()) |>
    relocate(Prevalence, .after = last_col())
  # Format the table:
  gtab <-
    tab |>
    select(-perc) |>
    gt_apa_table(tit = '<b>Table 3</br>
    Estimates of prevalence.</b> Estimates of the prevalence of probable PD-D in the sample.'
    ) |>
    tab_spanner(
      columns = c('Attention', 'Executive', 'Construction', 'Memory', 'Language'),
      label   = 'Impaired cognition',
      gather  = F
    ) |>
    cols_label(
      Global    ~ 'Global deficit',
      Executive ~ 'Executive function',
      IADL      ~ 'Impact on IADLs'
    )
  # If there are notes, add them to the table:
  if (ncol(v) > 4 && any(!is.na(v$note))) {
    notes <- v[complete.cases(v[ , 5]), c(2,5)]
    text  <- paste0(pull(notes[ , 1]),': ',pull(notes[ , 2])) |> paste(collapse = ', ')
    # Add the text:
    gtab <-
      gtab |>
      tab_source_note(html(paste0('<i>Note.</i> ',text)))
  }
  # Visualisation code:
  smoca_9   <- subset(prevs, type == subset(d0$criteria, group == 'smoca' & iadl == 'faq_9')$type)$perc
  smoca_tot <- subset(prevs, type == subset(d0$criteria, group == 'smoca' & iadl == 'faq'  )$type)$perc
  plt <-
    prevs |>
    filter(!grepl('smoca', type)) |>
    mutate(
      kind = sapply(
        seq_len(length(type)),
        function(i) case_when(
          d0$criteria$glob[d0$criteria$type == type[i]] == 'mmse'       & d0$criteria$iadl[d0$criteria$type == type[i]] == 'faq_9' ~ 1,
          d0$criteria$glob[d0$criteria$type == type[i]] == 'mmse'       & d0$criteria$iadl[d0$criteria$type == type[i]] == 'faq'   ~ 2,
          d0$criteria$glob[d0$criteria$type == type[i]] == 'moca_total' & d0$criteria$iadl[d0$criteria$type == type[i]] == 'faq_9' ~ 3,
          d0$criteria$glob[d0$criteria$type == type[i]] == 'moca_total' & d0$criteria$iadl[d0$criteria$type == type[i]] == 'faq'   ~ 4
        )
      ),
      `Operationalised by:` = factor(
        x = kind,
        levels = 1:4,
        labels = c(
          'MMSE & FAQ (it. 9)',
          'MMSE & FAQ (total)',
          'MoCA & FAQ (it. 9)',
          'MoCA & FAQ (total)'
        ),
        ordered = F
      )
    ) |>
    ggplot() +
    aes(x = perc, colour = `Operationalised by:`, fill = `Operationalised by:`) +
    geom_histogram(aes(y = ..density..), colour = 'black', fill = 'white', bins = 30) +
    geom_density(lwd = 1, alpha = .25) +
    geom_vline(xintercept = smoca_9,   lwd = 1, lty = 'dashed', colour = 'orange2') +
    geom_vline(xintercept = smoca_tot, lwd = 1, lty = 'dashed', colour = 'blue') +
    labs(x = 'Estimated prevalence (%)', y = 'Density') +
    theme(legend.position = 'bottom')
  # Return:
  list(table = tab, gtable = gtab, plot = plt)
}
