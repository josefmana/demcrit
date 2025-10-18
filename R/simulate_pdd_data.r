#' Make Fake Data
#'
#' Prepares a minimal example of data in the right format for analysis via the demcrit
#' package functions. It is compulsory to specify FAQ summary data (mean, variance and
#' covariance, see below) and at least one additional global cognitive measure summary
#' data. The function then simulates `N` synthetic patients' PDD status via the
#' following steps:
#'
#' 1. generate latent FAQ and cognitive scores via [MASS::mvrnorm()],
#' 2. generate observed FAQ item 9 data via the [rbinom()] following tau-equivalence
#'    assumption applied to the FAQ questionnaire,
#' 3. censor the scores according to `cens` and round to nearest integer to obtain
#'    observed data
#' 4. apply criteria for PDD from `crits` to get per algorithm/patient pair probable
#'    PDD.
#'
#' @param N A numeric indicating number of simulated patients. Defaults to 203.
#' @param defaults A logical indicator whether default values derived from the
#'    observed data should be used (`TRUE`, default) or the user will provide their
#'    values instead (`FALSE`).
#' @param Mu Named numeric vector of means. Needs to contain one component denoting
#'    FAQ. If `NULL` (default), it is set to values from observed data..
#' @param Sigma Named matrix of variances and covariances. If `NULL` (default), it
#'    is set at empirical values from the observed data.
#' @param cens A matrix or data frame with two columns, the first indicating lower
#'    bound, the second the upper bound of a scale in the rowname. Defaults to
#'    `NULL` which does not censor data (and prints latent scores instead).
#' @param crits description
#'
#' @returns A data.frame
#'
#' @seealso [prepare_defaults()] for correct format of simulation parameters specification.
#'
#' @examples
#' \dontrun{
#' # Simulate using defaults:
#' sim1 <- simulate_pdd_data()
#' sim2 <- simulate_pdd_data(defaults = TRUE) # the same data-generating process as sim1
#'
#' # Generate using two cognitive measures not correlated with FAQ:
#' mu <- c(FAQ = 4.05, MMSE = 26.69, MoCA = 24.07)
#' sd <- c(FAQ = 4.89, MMSE = 2.22, MoCA = 3.48)
#' corrs <- matrix(
#'    c(1, 0, 0, 0, 1, 0.63, 0, 0.63, 1), nrow = 3,
#'    dimnames = lapply(seq_len(2), \(i) c("FAQ", "MMSE", "MoCA"))
#' )
#' cens <- matrix(
#'   c(rep(0, 3), rep(30, 3)), nrow = 3,
#'   dimnames = list(c("FAQ", "MMSE", "MoCA"))
#' )
#' sigma <-
#' crits <- data.frame(
#'   row.names = c("A1", "A2", "A3", "A4"),
#'   IADL = rep(c("FAQ", "FAQ9"), 2),
#'   IADL_thres = rep(c(7, 1), 2),
#'   cognition = c(rep("MMSE", 2), rep("MoCA", 2)),
#'   cognition_thres = c(rep(26, 2), rep(26, 2))
#' )
#' sim3 <- simulate_pdd_data(
#'   N = 2000,
#'   default = FALSE, # It is crucial to set this to FALSE
#'   Mu = mu, Sigma =
#' )
#' }
#'
#' @export
simulate_pdd_data <- function(N = 203, defaults = TRUE, Mu, Sigma, crits, cens = NULL) {
  # Set defaults
  if (defaults) {
    defs <- prepare_defaults()
    for (i in names(defs)) assign(i, defs[[i]])
  }
  # Check whether FAQ exists:
  cont_mu <- sum(grepl("FAQ", names(Mu))) == 1
  stopifnot("`Mu` must contain a component called FAQ." = cont_mu)
  cont_sigma <- sum(grepl("FAQ", dimnames(Sigma)[[1]])) == 1
  stopifnot("`Sigma` must contain a dimension called FAQ." = cont_mu)
  if (!is.null(cens)) {
    cont_cens <- sum(grepl("FAQ", rownames(cens))) == 1
    stopifnot("`cens` must contain a row called FAQ." = cont_cens)

  }
  # Generate latent data:
  dat <- as.data.frame(MASS::mvrnorm(N, mu = Mu, Sigma = Sigma))
  # Generate FAQ item 9 data:
  p <- pnorm(dat$FAQ, mean = Mu["FAQ"], sd = sqrt(Sigma["FAQ", "FAQ"]))
  dat$FAQ9 <- rbinom(N, size = 4, prob = p)
  dat$FAQ9 <- ifelse(dat$FAQ9 > dat$FAQ, 0, dat$FAQ9)
  # Bound scores realistically if censoring values were provided:
  if (!is.null(cens)) {
    for (i in seq_len(nrow(cens))) {
      dat[, i] <- pmin(pmax(as.integer(dat[,i]), cens[i, 1]), cens[i, 2])
    }
  }
  # Define PDD classifications:
  for (i in rownames(crits)) {
    dat[, i] <- dat[, crits[i, "IADL"]] > crits[i, "IADL_thres"] &
      dat[, crits[i, "cognition"]] < crits[i, "cognition_thres"]
  }
  # Return the data:
  dat |>
    tibble::add_column(id = glue::glue("s{seq_len(N)}"), .before = 1) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(rownames(crits)),
      names_to = "type",
      values_to = "PDD"
    )
}
