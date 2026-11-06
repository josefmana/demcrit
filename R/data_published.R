#' Published Algorithm Specifications and PDD Rates (v1.2.0)
#'
#' The version of \link{rates} distributed with demcrit v1.2.0, i.e., the
#' values printed in Mana et al. (2026). In these data, MoCA-based algorithms
#' applied the global criterion MoCA < 26, although Table 1 of the article
#' states MoCA < 27. Kept for comparison only; use \link{rates} for analyses.
#' See `vignette("corrections")` for details.
#'
#' @format A tibble with the same structure as \link{rates}.
#'
#' @seealso \link{rates} for the corrected data.
#'
#' @source demcrit v1.2.0 (commit 1843263).
"rates_published"

#' Published Table of Pairwise Concordance Measures (v1.2.0)
#'
#' The version of \link{concords} distributed with demcrit v1.2.0, i.e., the
#' values underlying the concordance results printed in Mana et al. (2026).
#' These statistics were computed on pairwise available cases (N between 190
#' and 202 per pair of algorithms) rather than on the 190 complete cases
#' described in the article, and with MoCA-based algorithms applying the
#' global criterion MoCA < 26. Kept for comparison only; use \link{concords}
#' for analyses. See `vignette("corrections")` for details.
#'
#' @format A tibble with the same structure as \link{concords}.
#'
#' @seealso \link{concords} for the corrected data.
#'
#' @source demcrit v1.2.0 (commit 1843263).
"concords_published"
