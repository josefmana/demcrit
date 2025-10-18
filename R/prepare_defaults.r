#' Prepare Defaults for Data Simulation
#'
#' Prepares a list with default values for data simulation. These defaults are
#' derived from the data used as a basis of the demcrit article. This function
#' is a helper with no inputs and is typically called within \code{simulate_PDD_data}.
#'
#' @returns A list containing:
#'   \describe{
#'     \item{\code{Mu}}{Named numeric vector containing means for FAQ, MMSE, MoCA,
#'     and sMoCA.}
#'     \item{\code{Sigma}}{Named matrix containing variance-covariance of the variables
#'     (FAQ, MMSE, MoCA, and sMoCA).}
#'     \item{\code{cens}}{A matrix with named rows denoting scales (FAQ, MMSE, MoCA,
#'     and sMoCA) and two columns - the first containing lower bound, the second
#'     containing upper bound of each scale.}
#'     \item{\code{crits}}{A data.frame with four columns denoting IADL measure to use
#'     (`IADL`), threshold for the IADL measure (`IADL_thres`), cognitive measure to
#'     use (`cognition`), and threshold for the cognitive measure (`cognition_thres`).
#'     Score above `IADL_thres` and below `cognition_thres` are regarder to indicate
#'     PDD symptoms. Rownames contain algorithm names.}
#'   }
#'
#' @seealso [simulate_pdd_data()] uses outputs of this function unless specified
#'    by the user.
#'
#' @export
prepare_defaults <- function() {
  # Data means:
  mu = c(FAQ = 4.05, MMSE = 26.69, MoCA = 24.07, sMoCA = 11.26)
  # Data variance-covariance matrix:
  sd <- c(FAQ = 4.89, MMSE = 2.22, MoCA = 3.48, sMoCA = 2.74)
  corrs <- matrix(
    c(1, -0.21, -0.27, -0.26, -0.21, 1, 0.63, 0.54, -0.27, 0.63, 1, 0.95, -0.26, 0.54, 0.95, 1),
    nrow = length(sd), # If the defaults ever changed.
    dimnames = lapply(seq_len(2), \(i) c("FAQ", "MMSE", "MoCA", "sMoCA"))
  )
  sigma <- cor2cov(corrs, sd)
  # Censoring values:
  cens <- matrix(
    c(rep(0, 4), rep(30, 3), 18),
    nrow = 4,
    dimnames = list(c("FAQ", "MMSE", "MoCA", "sMoCA"))
  )
  # Criteria:
  crits <- data.frame(
    row.names = c("MMSE (1)", "MMSE (2)", "MoCA (1)", "MoCA (2)", "sMoCA (1)", "sMoCA (2)"),
    IADL = rep(c("FAQ", "FAQ9"), 3),
    IADL_thres = rep(c(7, 1), 3),
    cognition = c(rep("MMSE", 2), rep("MoCA", 2), rep("sMoCA", 2)),
    cognition_thres = c(rep(26, 2), rep(26, 2), rep(12, 2))
  )
  # Return:
  list(Mu = mu, Sigma = sigma, cens = cens, crits = crits)
}
