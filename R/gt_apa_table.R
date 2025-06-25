#' Generate an APA-style table using the gt package
#'
#' Converts a tibble or data frame into an APA-formatted table using the
#' \pkg{gt} package. The resulting object inherits all the features and
#' customizations provided by \code{gt}.
#'
#' @param x A tibble or data frame to be formatted as an APA-style table.
#' @param grp An optional column name (as a string or symbol) to use for grouping
#' rows via \code{gt::gt()}'s \code{groupname_col} parameter. Defaults to \code{NULL}.
#' @param nms An optional column name (as a string or symbol) to use for row names
#' via \code{gt::gt()}'s \code{rowname_col} parameter. Defaults to \code{NULL}.
#' @param tit A character string specifying the table title. Will be converted to
#' HTML format. Defaults to \code{"No Name"}.
#'
#' @returns A \code{gt} table object formatted according to APA style.
#'
#' @export
gt_apa_table <- function(x, grp = NULL, nms = NULL, tit = "") {
  x |>
    gt(groupname_col = grp, rowname_col = nms) |>
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) |>
    cols_align(align = "center") |>
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align = "center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) |>
    tab_header( # title setup
      title = html(tit)
    ) |>
    opt_align_table_header(align = "left")
}
