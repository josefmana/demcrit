#' Diagnose PDD of a sample of patients using
#' one set of criteria.
#'
#' This function takes in a data matrix of
#' patients' data and a vector of criteria to
#' use and diagnoses each patient with PDD
#' (or not) according to the criteria. Ought
#' to be used within the wrapper function
#' \code{diagnose_pdd_sample}.
#'
#' @param x A patients data matrix.
#' @param c A vector with PDD
#' criteria.
#'
#' @returns
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#' crit <- specify_criteria()
#' pdd  <- diagnose_pdd_case(data, crit[1, ])
#' }.
#'
#' @export
diagnose_pdd_case <- function(x, c) {
  c[ , c(is.na(c))] <- 'drsii' # DRS-2 as a dummy variable.
  x |>
    mutate(
      impaired_orig_att = if_else(x[[c$atte]] < c$atte_t, T, F),
      impaired_orig_exe = if_else(x[[c$exec]] < c$exec_t, T, F),
      impaired_orig_con = if_else(x[[c$cons]] < c$cons_t, T, F),
      impaired_orig_mem = if_else(x[[c$memo]] < c$memo_t, T, F),
      impaired_new_lan  = if_else(x[[c$lang]] < c$lang_t, T, F),
      # PDD criteria proper:
      crit1 = T, # 1. Parkinson’s disease
      crit2 = T, # 2. Parkinson’s disease developed before dementia
      crit3 = if_else(x[[c$glob]] < c$glob_t, T, F), # 3. Global deficit
      crit4 = if_else(x[[c$iadl]] > c$iadl_t, T, F), # 4. Dementia has Impact on ADLs
      crit5 = case_when( # 5. Impaired cognition (for Yes, at least of 2 tests abnormal)
        c$group == 'mmse'  ~ if_else(rowSums(across(starts_with('impaired_orig'))) > 1, T, F),
        c$group == 'moca'  ~ if_else(rowSums(across(starts_with('impaired')))      > 1, T, F),
        c$group == 'smoca' ~ crit3
      ),
      crit6 = T, # 6. Absence of Major Depression
      crit7 = T, # 7. Absence of Major Depression
      crit8 = T, # 8. Absence of other abnormalities that obscure diagnosis
      PDD   = if_else(rowSums(across(starts_with('crit'))) == 8, T, F)
    ) |>
    select(id, PDD, starts_with('impaired'), starts_with('crit'))
}
