#' Generate a summary table of PDD criteria used in the study
#'
#' This function creates a pre-specified summary table of the probable PDD
#' criteria compared in the study. It is intended as a shortcut to insert
#' a clean table into a Quarto document without cluttering the main text.
#' The table content is hardcoded and can only be changed by modifying the function code.
#'
#' @param vars A data.frame, tibble, or matrix with variable names in the first column,
#' variable labels in the second column, variable type (continuous, binary, or nominal)
#' in the third column, an optional fourth column indicating group, and a fifth column
#' mapping each label to a note or description. Alternatively, a path to a semicolon-delimited
#' CSV file containing such a table.
#'
#' @returns A \code{gt} table object containing the summary of criteria used in the study.
#'
#' @export
table_criteria <- function(vars) {
  # Prepare the note:
  if (is.character(vars)) {
    v <- readr::read_delim(vars, delim = ";", col_types = cols())
  } else {
    v <- vars
  }
  v <- subset(v, group %in% c("Attention and Working Memory", "Executive Function", "Language", "Memory", "Visuospatial Function"))
  note <- paste0(
    "Note. MMSE: Mini-Mental State Examination; MoCA: Montreal Cognitive Assessment; sMoCA: short version of the MoCA; ",
    paste0(v$label,": ",v$note) |> paste(collapse = "; "),
    ". The OR operator implies that exactly one of the criteria listed is utilized within a single operationalization;
      the & operator implies that both criteria are used at the same time within a single operationalization;
      each threshold value within the set brackets {} was used to define probable PDD once in combination with all the
      other criteria on the same row."
  )
  # Do the table:
  tibble(
    Type = c(
      rep("MMSE-based", 5),
      rep("MoCA-based", 6),
      rep("sMoca-based", 1),
      rep("Level II", 5)
    ),
    Domain = c(
      "Global functioning", "Attention", "Executive Function", "Construction", "Memory",
      "Global functioning", "Attention", "Executive Function", "Construction", "Memory", "Language",
      "Global functioning",
      "Attention", "Executive Function", "Construction", "Memory", "Language"
    ),
    Indexes = c(
      "MMSE < 26", "Sevens backwards < 4", "Clock drawing < 2 OR Lexical fluency (S) < 10", "Pentagons < 1", "3-word recall < 3",
      "MoCA < 27", "Sevens backwards < 3", "Clock drawing < {2, 3} OR Lexical fluency (K) < 11", "Cube drawing < 1", "5-word recall < {1, 2, 3, 4, 5}", "Abstraction < 2 OR Animal naming < 3",
      "sMoCA < 13",
      "TMT A & WAIS DSB", "CF A & PST C", "JoLO & CLOXI", "RAVLT DR & (BVMTR DR OR WMS-III Family Pictures DR)", "WAIS Similarities & BNT 60"
    )
  ) |>
    pivot_wider(names_from = Domain, values_from = Indexes) |>
    gt_apa_table() |>
    sub_missing(missing_text = "-") |>
    tab_spanner(
      columns = all_of(c("Attention", "Executive Function", "Construction", "Memory", "Language")),
      label = "Impaired Cognition"
    ) |>
    tab_source_note(note) |>
    tab_footnote(
      locations = cells_body(rows = 4, columns = Memory),
      footnote = "The visual memory was evaluated based on WMS-III Family Pictures or BVMTR depending on which
      test was used in the assessment. This lead to no missing values because each patient underwent assessment
      via one of these tests."
    ) |>
    opt_footnote_marks(marks = "letters")
}
