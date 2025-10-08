#' Specify criteria operationalisations for PDD diagnosis
#'
#' Computes a Cartesian product of possible diagnostic components to define
#' all combinations of operationalised criteria for probable Parkinson’s disease
#' dementia (PDD). This function is a helper and is typically called within
#' \code{diagnose_pdd_sample}.
#'
#' @returns A tibble where each row represents one unique operationalisation
#'    of PDD diagnostic criteria. Columns indicate the type of criteria, individual
#'    test indexes, thresholds, and other relevant parameters.
#'
#' @export
specify_criteria <- function() {
  # MMSE-based criteria
  mmse <- crossing(exec = c("vf_s","cloc"), iadl = c("faq_9","faq")) |>
    mutate(
      group = "mmse",
      type = paste0("MMSE (",seq_len(n()),")"),
      glob = "mmse",
      glob_t = 26,
      atte = "mmse_7",
      atte_t = 4,
      memo = "mmse_3words",
      memo_t = 3,
      cons = "mmse_pent",
      cons_t = 1,
      lang = NA,
      lang_t = 0,
      exec_t = case_when(exec == "cloc" ~ 2, exec == "vf_s" ~ 10),
      iadl_t = case_when(iadl == "faq"  ~ 7, iadl == "faq_9" ~ 1)
    ) |>
    relocate(exec, .after = atte_t) |>
    relocate(exec_t, .after = exec) |>
    relocate(iadl, .before = iadl_t)
  # MoCA-based criteria
  moca <- map_dfr(2:3, function(i) {
    crossing(
      group = "moca",
      glob = "moca_total",
      glob_t = 26,
      atte = "moca_7",
      atte_t = 3,
      exec = c("moca_cloc", "vf_k"),
      memo = "moca_5words",
      memo_t = 5:1,
      cons = "moca_cube",
      cons_t = 1,
      lang = c("moca_anim", "moca_abs"),
      iadl = c("faq", "faq_9")
    ) |>
      mutate(
        exec_t = case_when(exec == "vf_k" ~ 11, exec == "moca_cloc" ~ i),
        iadl_t = case_when(iadl == "faq" ~ 7, iadl == "faq_9" ~ 1),
        lang_t = case_when(lang == "moca_abs" ~ 2 , lang == "moca_anim" ~ 3)
      )
  }) |>
    mutate(type = paste0("MoCA (", seq_len(n()), ")"))
  # sMoCA-based criteria
  smoca <- crossing(
    group = "smoca",
    glob = "smoca_total",
    glob_t = 13,
    iadl = c("faq_9","faq")
  ) |>
    mutate(
      type = paste0("sMoCA (", seq_len(n()), ")"),
      iadl_t = case_when(iadl == "faq" ~ 7, iadl == "faq_9" ~ 1)
    )
  # Level II-based criteria
  lvlII <- crossing(
    group = "lvlII",
    glob = "nonCI",
    glob_t = 1,
    iadl = c("faq_9","faq")
  ) |>
    mutate(
      type = paste0("Lvl.II (",seq_len(n()),")"),
      iadl_t = case_when(iadl  == "faq" ~ 7, iadl == "faq_9" ~ 1)
    )
  # Return all-in-one:
  map_dfr(list(mmse, moca, smoca, lvlII), \(x) x) |>
    mutate(across(ends_with("_t"), \(x) if_else(is.na(x), 0, x)))
}
