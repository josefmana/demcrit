# Make Fake Data

Prepares a minimal example of data in the right format for analysis via
the demcrit package functions. It is compulsory to specify FAQ summary
data (mean, variance and covariance, see below) and at least one
additional global cognitive measure summary data.

## Usage

``` r
simulate_pdd_data(N = 203, defaults = TRUE, Mu, Sigma, crits, cens = NULL)
```

## Arguments

- N:

  A numeric indicating number of simulated patients. Defaults to 203.

- defaults:

  A logical indicator whether default values derived from the observed
  data should be used (`TRUE`, default) or the user will provide their
  values instead (`FALSE`).

- Mu:

  Named numeric vector of means. Needs to contain one component denoting
  FAQ. If `defaults = TRUE`, it is set to values from observed data.

- Sigma:

  Named matrix of variances and covariances. If `defaults = TRUE`, it is
  set at empirical values from the observed data.

- crits:

  A data.frame with named rows and four columns:

  - rownames indicate algorithm labels,

  - `IADL` indicate variable from the data used for IADL deficit
    calculation,

  - `IADL_thres` indicate threshold above which there is IADL deficit,

  - `cognition` indicate variable from the data used for cognitive
    deficit calculation,

  - `cognition_thres` indicate threshold below which there is cognitive
    deficit.

- cens:

  A matrix or data frame with two columns, the first indicating lower
  bound, the second the upper bound of a scale in the rowname. If
  `defaults == TRUE`, or `param = NULL` which does not censor data (and
  prints latent scores instead).

## Value

A tibble containing:

- id:

  Character; identificator of a simulated patient.

- type:

  Character; algorithm names specified by rownames of `crits`

- PDD:

  Logical; PDD diagnosis for each patient (`id`) calculated via each
  algorithm (`type`),

- raw scores:

  Numeric; Scores showing raw simulated data for each variable supplied
  to the data-generating proces. Defaults to columns `FAQ`, `MMSE`,
  `MoCA`, `sMoCA` and `FAQ9`.

## Details

The function simulates `N` synthetic patients' PDD status via the
following steps:

1.  generate latent FAQ and cognitive scores via
    [`MASS::mvrnorm()`](https://rdrr.io/pkg/MASS/man/mvrnorm.html),

2.  generate observed FAQ item 9 data via the
    [`rbinom()`](https://rdrr.io/r/stats/Binomial.html) following
    tau-equivalence assumption applied to the FAQ questionnaire,

3.  censor the scores according to `cens` and round to nearest integer
    to obtain observed data

4.  apply criteria for PDD from `crits` to get per algorithm/patient
    pair probable PDD.

## See also

[`prepare_defaults()`](https://josefmana.github.io/demcrit/reference/prepare_defaults.md)
for correct format of simulation parameters specification.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulate using defaults:
sim1 <- simulate_pdd_data()
sim2 <- simulate_pdd_data(defaults = TRUE) # the same data-generating process as sim1

# Generate using two cognitive measures not correlated with FAQ:
mu <- c(FAQ = 4.05, MMSE = 26.69, MoCA = 24.07)
sd <- c(FAQ = 4.89, MMSE = 2.22, MoCA = 3.48)
corrs <- matrix(
   c(1, 0, 0, 0, 1, 0.63, 0, 0.63, 1), nrow = 3,
   dimnames = lapply(seq_len(2), \(i) c("FAQ", "MMSE", "MoCA"))
)
cens <- matrix(
  c(rep(0, 3), rep(30, 3)), nrow = 3,
  dimnames = list(c("FAQ", "MMSE", "MoCA"))
)
sigma <- MBESS::cor2cov(corrs, sd)
crits <- data.frame(
  row.names = c("A1", "A2", "A3", "A4"),
  IADL = rep(c("FAQ", "FAQ9"), 2),
  IADL_thres = rep(c(7, 1), 2),
  cognition = c(rep("MMSE", 2), rep("MoCA", 2)),
  cognition_thres = c(rep(26, 2), rep(26, 2))
)
sim3 <- simulate_pdd_data(
  N = 2000,
  defaults = FALSE, # It is crucial to set this to FALSE
  Mu = mu, Sigma = sigma
)
} # }
```
