#' Calculate a set of logistic regressions predicting
#' probable PDD by age (did not include).
#'
#' This function takes in two datasets - one containing
#' demographic variables (got via the \code{prepare_data}
#' function) and one containing probable PDD diagnosis
#' according to operationalisations used (PDD tibble
#' from the list created by the \code{diagnose_pdd_sample}
#' function) - and for each operationalisation used
#' computes logistic regression of probable PDD regressed
#' on age, sex, and their interaction.
#'
#' @param d0 A tibble with demographic data from \code{prepare_data}.
#' @param d1 A tibble with PDD data from \code{diagnose_pdd_sample}.
#'
#' @returns A list containing all logistic regression
#' models ($fits), parameter values ($values) and a list
#' with ggplots showing raw data ($data), and summarising
#' effect sizes (Odds Ratios) and p-values ($parameters).
#'
#' @examples
#' \dontrun{
#' p <- data_paths("data-raw")
#' data0 <- prepare_data(p)
#' data1 <- diagnose_pdd_sample(data0)
#' predictions <- regress_pdd_on_demographics(data0, data1$PDD)
#' }
#' @export
regress_pdd_on_demographics <- function(d0, d1) {
  # Extract orders for visualisation:
  ord_id <- d0 |> arrange(age) |> filter(!is.na(age)) |> select(id) |> pull()
  ord_type <- d1 |> select(PDD, type) |> table() |> prop.table() |> as_tibble() |> filter(PDD == T) |> arrange(n) |> select(type) |> pull()
  # Prepare an anonimisation mapping:
  anon <- left_join(
    data.frame(id = ord_id),
    cbind.data.frame(
      id = unique(d0$id),
      sid = paste0("S", sprintf("%03d", seq_len(length(unique(d0$id)))))
    ),
    by ="id"
  )
  # Prepare data for analysis:
  df <-
    d1 |>
    select(id, type, PDD) |>
    left_join(d0 |> select(id, age, sex), by = "id") |>
    mutate(
      PDD = if_else(PDD, 1, 0),
      sid = factor(
        sapply(
          seq_len(length(id)),
          function(i)
            anon$sid[anon$id == id[i]]
        ),
        levels = anon$sid,
        ordered = T
      ),
      type = factor(type, levels = ord_type, ordered = T)
    )
  # Visualise raw data:
  plt_raw <-
    df |>
    filter(!is.na(sid)) |>
    ggplot() +
    aes(x = sid, y = type, fill = factor(PDD)) +
    geom_tile() +
    scale_fill_manual(values = c("grey89", "red3"), na.value = "white") +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none") +
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = .5,
        colour = case_when(
          df$sex == "male" ~ "steelblue",
          df$sex == "female" ~ "red2"
        )
      )
    ) +
    NULL
  # Get regression fits:
  fits <-
    lapply(
      set_names(unique(df$type)),
      function(i) glm(
        formula = PDD ~ age * sex,
        data = subset(df, type == i),
        family = binomial(link = "logit")
      )
    )
  # Get parameters:
  pars <-
    sapply(
      set_names(names(fits)),
      function(i) c(
        or_ = exp(coefficients(fits[[i]])),
        p_ = summary(fits[[i]])$coefficients[ , "Pr(>|z|)"]
      )
    ) |>
    t()|>
    as_tibble(rownames = "type") |>
    select(-contains("Intercept")) |>
    pivot_longer(
      cols = -type,
      names_to = c("quantity", "predictor"),
      names_sep = "_",
      values_to = "estimate"
    ) |>
    mutate(
      quantity = case_when(
        quantity == "or" ~ "Odds Ratio",
        quantity == "p" ~ "p-value"
      ),
      predictor = factor(
        case_when(
          predictor == ".age" ~ "Age",
          predictor == ".sexmale" ~ "Sex",
          predictor == ".age:sexmale" ~ "Age * Sex"
        ),
        levels = c("Age", "Sex", "Age * Sex"),
        ordered = T
      ),
      vline_or = if_else(quantity == "Odds Ratio", 1, NA),
      vline_p = if_else(quantity == "p-value", .05, NA)
    )
  # Plot regression results:
  plt_regrs <-
    pars |>
    filter(estimate < 20) |>
    ggplot() +
    aes(x = estimate) +
    geom_histogram(fill = "white", colour = "black", bins = 15) +
    geom_vline(aes(xintercept = vline_or), linetype = "dashed", linewidth = .8, colour = "blue3") +
    geom_vline(aes(xintercept = vline_p), linetype = "dashed", linewidth = .8, colour = "red3") +
    labs(x = NULL, y = NULL) +
    ggh4x::facet_grid2(quantity ~ predictor, scales = "free", independent = "all") +
    NULL
  # Return it:
  list(fits = fits, values = pars, plots = list(data = plt_raw, parameters = plt_regrs))
}
