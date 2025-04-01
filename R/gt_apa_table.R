#' Get APA-style gt table.
#'
#' This function takes in a tibble or a data.frame
#' and returns APA-style table created via the gt
#' package. All gt quirks and perks apply to the
#' resulting object.
#'
#' @param x A tibble/data.frame to be APAed.
#' @param grp A column name to be applied to
#' the gt::gt() groupname_col parameter (NULL
#' by default).
#' @param nms A column name to be applied to
#' the gt::gt() rowname_col parameter (NULL
#' by default).
#' @param tit A character including table name.
#' Will be forced to become html (No Name by default).
#'
#' @returns A gt object of a table in APA format.
#'
#' @export
gt_apa_table <- function(x, grp = NULL, nms = NULL, tit = '') {
  x |>
    gt(groupname_col = grp, rowname_col = nms) |>
    tab_options(
      table.border.top.color            = 'white',
      heading.title.font.size           = px(16),
      column_labels.border.top.width    = 3,
      column_labels.border.top.color    = 'black',
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = 'black',
      table_body.border.bottom.color    = 'black',
      table.border.bottom.color         = 'white',
      table.width                       = pct(100),
      table.background.color            = 'white'
    ) |>
    cols_align(align = 'center') |>
    tab_style(
      style = list(
        cell_borders(
          sides  = c('top', 'bottom'),
          color  = 'white',
          weight = px(1)
        ),
        cell_text(
          align='center'
        ),
        cell_fill(color = 'white', alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows    = everything()
      )
    ) |>
    tab_header( # title setup
      title = html(tit)
    ) |>
    opt_align_table_header(align = 'left')
}
