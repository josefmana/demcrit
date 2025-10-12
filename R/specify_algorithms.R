#' Specify Algorithms for Probable PDD
#'
#' Computes a Cartesian product of possible diagnostic components to define
#' all combinations of algorithms for probable Parkinson’s disease dementia (PDD).
#' This function is a helper with no inputs and is typically called within
#' \code{diagnose_pdd_sample}.
#'
#' @returns A tibble where each row represents one unique diagnostic algorithm
#'    for probable PDD. Columns indicate the type of algorithms, individual test
#'    indexes, thresholds, and other relevant parameters.
#'
#' @seealso [diagnose_pdd_sample()] combines the algorithms with data to diagnose
#'    single patients.
#'
#' @export
specify_algorithms <- function() {
  # MMSE-based algorithms
  mmse <- tidyr::crossing(exec = c("vf_s","cloc"), iadl = c("faq_9","faq")) |>
    dplyr::mutate(
      group = "mmse",
      type = paste0("MMSE (",seq_len(dplyr::n()),")"),
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
      exec_t = dplyr::case_when(exec == "cloc" ~ 2, exec == "vf_s" ~ 10),
      iadl_t = dplyr::case_when(iadl == "faq"  ~ 7, iadl == "faq_9" ~ 1)
    ) |>
    dplyr::relocate(exec, .after = atte_t) |>
    dplyr::relocate(exec_t, .after = exec) |>
    dplyr::relocate(iadl, .before = iadl_t)
  # MoCA-based algorithms
  moca <- purrr::map_dfr(2:3, function(i) {
    tidyr::crossing(
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
        exec_t = dplyr::case_when(exec == "vf_k" ~ 11, exec == "moca_cloc" ~ i),
        iadl_t = dplyr::case_when(iadl == "faq" ~ 7, iadl == "faq_9" ~ 1),
        lang_t = dplyr::case_when(lang == "moca_abs" ~ 2 , lang == "moca_anim" ~ 3)
      )
  }) |>
    dplyr::distinct() |>
    mutate(type = paste0("MoCA (", seq_len(n()), ")"))
  # sMoCA-based algorithms
  smoca <- tidyr::crossing(
    group = "smoca",
    glob = "smoca_total",
    glob_t = 13,
    iadl = c("faq_9","faq")
  ) |>
    dplyr::mutate(
      type = paste0("sMoCA (", seq_len(dplyr::n()), ")"),
      iadl_t = dplyr::case_when(iadl == "faq" ~ 7, iadl == "faq_9" ~ 1)
    )
  # Level II-based algorithms
  lvlII <- tidyr::crossing(
    group = "lvlII",
    glob = "nonCI",
    glob_t = 1,
    iadl = c("faq_9","faq")
  ) |>
    dplyr::mutate(
      type = paste0("Lvl.II (",seq_len(dplyr::n()),")"),
      iadl_t = dplyr::case_when(iadl  == "faq" ~ 7, iadl == "faq_9" ~ 1)
    )
  # Return all-in-one:
  purrr::map_dfr(list(mmse, moca, smoca, lvlII), \(x) x) |>
    mutate(dplyr::across(tidyselect::ends_with("_t"), \(x) dplyr::if_else(is.na(x), 0, x)))
}
