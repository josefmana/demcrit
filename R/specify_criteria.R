#' Compute a cartesian product to specify
#' criteria operationalisations.
#'
#' This is purely a helper function that
#' specifies a tibble with specifications
#' of single algorithms for PDD diagnosis.
#'
#' @returns A tibble with columns indicating
#' type of criterion as well as single indexes
#' and thresholds. To be used within a wrapper
#' function \code{diagnose_pdd_sample}.
#'
#' @export
specify_criteria <- function() {
  # MMSE-based criteria
  mmse <-
    crossing(exec = c('vf_s','cloc'), iadl = c('faq_9','faq')) |>
    mutate(
      group  = 'mmse',
      type   = paste0('mmse',seq_len(n())),
      glob   = 'mmse',
      glob_t = 26,
      atte   = 'mmse_7',
      atte_t = 4,
      memo   = 'mmse_3words',
      memo_t = 3,
      cons   = 'mmse_pent',
      cons_t = 1,
      lang   = NA,
      lang_t = 0,
      exec_t = case_when(exec == 'cloc' ~ 2, exec == 'vf_s' ~ 10),
      iadl_t = case_when(iadl == 'faq'  ~ 7, iadl == 'faq_9' ~ 1)
    ) |>
    relocate(exec  , .after  = atte_t) |>
    relocate(exec_t, .after  = exec  ) |>
    relocate(iadl  , .before = iadl_t)
  # MoCA-based criteria
  moca <-
    lapply(
      2:3,
      function(i)
        crossing(
          group  = 'moca',
          glob   = 'moca_total',
          glob_t = 26,
          atte   = 'moca_7',
          atte_t = 3,
          exec   = c('moca_cloc', 'vf_k'),
          memo   = 'moca_5words',
          memo_t = 5:1,
          cons   = 'moca_cube',
          cons_t = 1,
          lang   = c('moca_anim', 'moca_abs'),
          iadl   = c('faq', 'faq_9')
        ) |>
        mutate(
          exec_t = case_when(exec  == 'vf_k'     ~ 11, exec == 'moca_cloc' ~ i),
          iadl_t = case_when(iadl  == 'faq'      ~ 7 , iadl == 'faq_9'     ~ 1),
          lang_t  = case_when(lang == 'moca_abs' ~ 2 , lang == 'moca_anim' ~ 3)
        )
    ) |>
    reduce(full_join) |>
    suppressMessages() |>
    mutate(type = paste0('moca',seq_len(n())))
  # sMoCA-based criteria
  smoca <-
    crossing(
      group  = 'smoca',
      glob   = 'smoca_total',
      glob_t = 13,
      iadl   = c('faq_9','faq')
    ) |>
    mutate(
      type   = paste0('smoca',seq_len(n())),
      iadl_t = case_when(iadl  == 'faq' ~ 7 , iadl == 'faq_9' ~ 1)
    )
  # Level II-based criteria
  lvlII <-
    crossing(
      group  = 'lvlII',
      glob   = 'nonCI',
      glob_t = 1,
      iadl   = c('faq_9','faq')
    ) |>
    mutate(
      type   = paste0('lvlII',seq_len(n())),
      iadl_t = case_when(iadl  == 'faq' ~ 7 , iadl == 'faq_9' ~ 1)
    )
  # Return all-in-one:
  list(mmse, moca, smoca, lvlII) |>
    reduce(full_join) |>
    suppressMessages() |>
    mutate(across(ends_with('_t'), ~if_else(is.na(.x), 0, .x)))
}
