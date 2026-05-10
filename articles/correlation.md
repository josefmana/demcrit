# Impact of Correlation on Concordance

Code

``` r

# Load packages needed for this report:
library(demcrit)
library(tidyverse) # dor data wrangling
library(MASS)      # for mvrnorm()
library(caret)     # for confusionMatrix()
library(psych)     # for cohen.kappa()
library(gt)        # for table formatting
```

During the article’s review process, the following question arose:

> How can **correlation** between FAQ scores and neuropsychological
> measures **inflate** concordance estimates?

## Introduction

Because this question is not a straightforward one to address
conceptually, we conducted a brief simulation experiment to explore it
empirically.

This vignette presents the R code and results of a simulation study
comparing algorithms’ concordance and PDD rate estimates in a simplified
setting. In this scenario, the only differences between algorithms are
the operationalisations of IADL deficit (FAQ \> 7 vs. FAQ item 9 \< 1)
and global cognitive deficit (MMSE \< 26 vs. MoCA \< 26), while all
other diagnostic criteria are assumed to be satisfied.

Crucially, **within algorithms**, we vary correlation between FAQ and
MMSE/MoCA in the data-generating process, allowing us to compare
estimates under two conditions: one with no correlation (**independent**
scenario) and one with a correlation of the size consistent with that
observed in our data (**correlated** scenario, see
[Table 1](#tbl-data)).

[TABLE]

Table 1: Distribution moments (means and standard deviations) and
Pearson’s correlations between FAQ, MMSE and MoCA as measured in the
observed data.

## Data simulation

Code

``` r

# Simulation parameters:
n <- 2000 # sample size high enough to get stable estimates
k <- 2000 # enough iterations to see trends 

# Moments of the multivariate normal distribution:
mu <- c(FAQ = 4.05, MMSE = 26.69, MoCA = 24.07) # vector of means
sigma <- c(FAQ = 4.89, MMSE = 2.22, MoCA = 3.48) # vector of standard deviations

# Correlation matrix from observed data:
corrs <- matrix(
  c(1, -0.21, -0.27,
    -0.21, 1, 0.63,
    -0.27, 0.63, 1),
  nrow = 3,
  dimnames = list(
    c("FAQ", "MMSE", "MoCA"),
    c("FAQ", "MMSE", "MoCA")
  )
)

# Get variance-covariance matrix for the correlated case:
sigma_corr <- cor2cov(corrs, sigma)

# Set FAQ/cognition covariances to zero for the independent case:
sigma_indep <- sigma_corr
sigma_indep["FAQ", ] <- c(sigma_indep["FAQ", "FAQ"], 0, 0)
sigma_indep[, "FAQ"] <- c(sigma_indep["FAQ", "FAQ"], 0, 0)

# Set censoring values:
cens <- matrix(
  c(rep(0, 3), rep(30, 3)), nrow = 3,
  dimnames = list(c("FAQ", "MMSE", "MoCA"))
)

# Set criteria:
crits <- data.frame(
  row.names = c("A1", "A2", "A3", "A4"),
  IADL = rep(c("FAQ", "FAQ9"), 2),
  IADL_thres = rep(c(7, 1), 2),
  cognition = c(rep("MMSE", 2), rep("MoCA", 2)),
  cognition_thres = c(rep(26, 2), rep(26, 2))
)
```

### FAQ, MMSE & MoCA

To approximate the situation examined in the study, we simulate FAQ,
MMSE and MoCA data from a common multivariate. In other words, each
subject $`i`$ was assumed to have the following vector of latent scores:

``` math
 Y_i = \begin{bmatrix} FAQ_i \\ MMSE_i \\ MoCA_i \end{bmatrix} \sim \mathcal{MVN_3}(\boldsymbol{\mu}, \boldsymbol{\Sigma})
 \qquad(1)
```

where $`\boldsymbol\mu`$ denotes the vector of means and
$`\boldsymbol\Sigma`$ the variance–covariance matrix. Across all
simulations, the vector of means was fixed based on observed data (see
[Table 1](#tbl-data)):

``` math
\boldsymbol\mu = \begin{bmatrix} 4.05 \\ 26.69 \\ 24.07 \end{bmatrix}
 \qquad(2)
```

Two different specification of the variance-covariance matrix were used.
The first, based on observed data ([Table 1](#tbl-data)), represents the
**correlated** scenario:

``` math
\boldsymbol\Sigma_{corr} =
  \begin{bmatrix} 23.91 & -2.28 & -4.59 \\
                  -2.28 &  4.93 & 4.87  \\
                  -4.59 &  4.87 & 24.07
  \end{bmatrix}
 \qquad(3)
```

To simulate the **independent** scenario, covariance between FAQ and
MMSE/MoCA was set to zero, yielding:

``` math
\boldsymbol\Sigma_{indep} =
  \begin{bmatrix} 23.91 &  0    & 0    \\
                   0    &  4.93 & 4.87  \\
                   0    &  4.87 & 24.07
  \end{bmatrix}
 \qquad(4)
```

### FAQ item 9

To simulate data for FAQ item 9, we assumed
$`\boldsymbol{\tau}`$**-equivalence** of FAQ (i.e., that all items share
the same true score, cf. Trizano-Hermosilla and Alvarado
([2016](#ref-trizano-hermosilla2016))). We then applied probit link to
the FAQ total score of each simulated patient to obtain the probability
of a positive response to any item:

``` math
p_i = \phi(z_i)
 \qquad(5)
```

where $`\phi(\cdot)`$ is the standard normal cumulative distribution
function and $`z_i`$ is the standardised FAQ score for simulated patient
$`i`$. The observed FAQ item 9 score was subsequently drawn from a
binomial distribution via:

``` math
\text{FAQ item 9}_i \sim \mathcal{Binomial}(n = 4, p_i)
 \qquad(6)
```

If the simulated FAQ item 9 score exceeded the corresponding simulated
FAQ total score, it was set at zero. Finally, all simulated FAQ total,
MMSE and MoCA scores were left-censored at 0 and right-censored at 30 to
reflect real-world range of the respective scales.

## Diagnostic algorithms

After generating the data using the process described above, each
simulated patient was classified as suffering from PDD (or not)
according to all of the following four algorithms:

``` math
\begin{aligned}
  A1_i &= \mathbb{1}\{\text{FAQ}_i > 7 \ \wedge\ \text{MMSE}_i < 26\}, \\
  A2_i &= \mathbb{1}\{\text{FAQ item 9}_i > 1 \ \wedge\ \text{MMSE}_i < 26\}, \\
  A3_i &= \mathbb{1}\{\text{FAQ}_i > 7 \ \wedge\ \text{MoCA}_i < 26\}, \\
  A4_i &= \mathbb{1}\{\text{FAQ item 9}_i > 1 \ \wedge\ \text{MoCA}_i < 26\}
\end{aligned}
 \qquad(7)
```

where $`\mathbb{1}\{\cdot\}`$ denotes the indicator function. In other
words, simulated cases fulfilling both conditions in parentheses were
classified as having PDD (PDD = 1); all others were classified as
non-PDD (PDD = 0).

### Data

To ensure reliable results, N = 2000 cases were generated in this
experiment. The simulation was based on the observed means, standard
deviations and correlations as specified above. The **independent
variable** in this experiment was presence versus absence of correlation
between FAQ and cognitive scores. Accordingly, the simulation was
conducted under two conditions:

- **Independent** scenario using $`\boldsymbol\Sigma_{indep}`$
  ([Equation 4](#eq-sigma-indep)),
- **Correlated** scenario using $`\boldsymbol\Sigma_{corr}`$
  ([Equation 3](#eq-sigma-corr)).

All other simulation parameters (i.e., the number of participants, the
vector of variable means, the variances of all variables, and the
covariance between MMSE and MoCA) were held constant across conditions.

## Statistical analysis

Code

``` r

# Function for comparing two algorithms within the same scenario:
compare_algos <- function(dat, a1, a2) {
  x <- dat[[a1]]
  y <- dat[[a2]]
  cm <- caret::confusionMatrix(factor(x), factor(y), positive = "1")
  k <- psych::cohen.kappa(cbind(x, y))$kappa
  data.frame(
    Accuracy = cm$overall["Accuracy"],
    BalAcc = cm$byClass["Balanced Accuracy"],
    Kappa = k
  )
}

# Function for results summary:
summarise_results <- function(d0, d1, r = 1) {
  # PDD rates:
  rates <- map_dfr(paste0("A", 1:4), function(i) {
    c(indep = mean(d0[[i]]),
      corrs = mean(d1[[i]])
      )
  }) |>
    mutate(diff = corrs - indep, rep  = r) |>
    rownames_to_column("algo")
  # Concordance:
  conc <- expand.grid(
    ref = paste0("A", 1:4),
    pred = paste0("A", 1:4),
    scenario = c("indep", "corrs")
  )
  for (i in c("Accuracy", "BalAcc", "Kappa")) {
    conc[[i]] <- NA
  }
  for (i in seq_len(nrow(conc))) {
    y <- as.character(conc[i, "ref"])
    x <- as.character(conc[i, "pred"])
    if (conc[i, "scenario"] == "indep") {
      conc[i , c("Accuracy", "BalAcc", "Kappa")] <- compare_algos(d0, x, y)
    } else if (conc[i, "scenario"] == "corrs") {
      conc[i, c("Accuracy", "BalAcc", "Kappa")] <- compare_algos(d1, x, y)
    }
  }
  conc$rep <- r
  # Return the results:
  list(rates = rates, concordance = as_tibble(conc))
}
```

After we generated the data, the following statistical estimates were
computed within each scenario (i.e., independent and correlated):

- **PDD rate** for each algorithm,
- **accuracy** for each pair of algorithms,
- **balanced accuracy** for each pair of algorithms,
- **Cohen’s** $`\boldsymbol\kappa`$ for each pair of algorithms.

This procedure was repeated for 2000 iterations. Subsequently, the
distributions of PDD rates and concordance measures were compared
between the same (pairs of) algorithms across the two scenarios.
Independent samples *t*-tests were used to compute 95% confidence
intervals for the differences between scenarios within each diagnostic
algorithm.

## Results

Code

``` r

# Calculate all results:
res <- lapply(seq_len(k), function(i) {
  d0 <- simulate_pdd_data(n, FALSE, mu, sigma_indep, crits, cens) |> # independent scenario
    mutate(PDD = as.numeric(PDD)) |>
    pivot_wider(names_from = "type", values_from = "PDD")
  d1 <- simulate_pdd_data(n, FALSE, mu, sigma_corr, crits, cens) |> # correlated scenario
    mutate(PDD = as.numeric(PDD)) |>
    pivot_wider(names_from = "type", values_from = "PDD")
  summarise_results(d0, d1, i)
})

# Extract PDD rates and concordance separately:
res_rates <- map_dfr(seq_len(k), \(i) res[[i]]$rates)
res_conc <- map_dfr(seq_len(k), \(i) res[[i]]$conc)

# Conduct t-tests for PDD rates:
stats_rates <- res_rates |>
  pivot_longer(c(indep, corrs), names_to = "scenario", values_to = "rate") |>
  group_by(algo) |>
  rstatix::t_test(rate ~ scenario, detailed = TRUE, var.equal = TRUE) |>
  mutate(
    Algorithm = glue::glue("A{algo}"),
    Independent = estimate2,
    Correlated = estimate1,
    Difference = estimate,
    Low = conf.low,
    High = conf.high
  ) |>
  dplyr::select(Algorithm, Independent, Correlated, Difference, Low, High)

# Conduct t-tests for concordance metrics:
stats_conc <- map_dfr(c("Accuracy", "BalAcc", "Kappa"), function(y) {
  res_conc |>
    filter(ref != pred) |>
    mutate(Comparison = glue::glue("{ref}{pred}")) |>
    group_by(Comparison) |>
    rstatix::t_test(formula(glue::glue("{y} ~ scenario")), detailed = TRUE, var.equal = TRUE) |>
    mutate(
      Index = y,
      Independent = estimate1,
      Correlated = estimate2,
      Difference = estimate,
      Low = conf.low,
      High = conf.high,
      Type = case_when(
        Comparison %in% c("A1A3", "A3A1", "A2A4", "A4A2") ~ "Aligned IADL & Mismatched Global Cognition",
        Comparison %in% c("A1A2", "A2A1", "A3A4", "A4A3") ~ "Mismatched IADL & Aligned Global Cognition",
        Comparison %in% c("A1A4", "A4A1", "A2A3", "A3A2") ~ "Mismatched IADL & Mismatched Global Cognition"
      )
    ) |>
    dplyr::select(Index, Type, Comparison, Independent, Correlated, Difference, Low, High)
})
```

The expected values (denoted $`E(\cdot)`$) and the differences between
the independent and correlated scenarios in PDD rates, accuracy,
balanced accuracy and Cohen’s $`\kappa`$ are presented in
[Table 2](#tbl-rates), [Table 3](#tbl-acc), [Table 4](#tbl-balacc), and
[Table 5](#tbl-kappa), respectively.

Across all algorithms, the **correlated** scenario generated **higher
PDD rates** than the independent scenario, with differences of
approximately 2-4% ([Table 2](#tbl-rates)).

[TABLE]

Table 2: Expectations of PDD rates and differences between the
independent and correlated scenarios based on independent sample
t-tests.

The effect of the data-generating scenario on **concordance indeces**
was more nuanced. For **accuracy**, the **correlated** scenario
generally yielded **slightly lower** estimates than the independent one
across most algorithm pairs ([Table 3](#tbl-acc)). However, the
magnitude of these differences did not exceed 2%.

[TABLE]

Table 3: Expectations of accuracy estimates and differences between the
independent and correlated scenarios based on independent sample
t-tests.

Results for **balanced accuracy** were more heterogeneous
([Table 4](#tbl-balacc)). In algorithm pairs where **both** the IADL and
global cognition criteria were **mismatched** (e.g., A1 vs A4), balanced
accuracy was consistently **higher** under the **correlated** scenario,
by roughly 3%.

When the only **mismatch** concerned the **IADL** criterion, the
**direction** of difference in balanced accuracy reversed depending on
which algorithm was used as the reference (e.g., when A1 predicted A2,
the correlated scenario yielded lower balanced accuracy, whereas when A2
predicted A1, it yielded higher values). The magnitude of this effect
was higher in the direction of correlated scenario yielding higher
estimates.

Finally, when the only **mismatch** involved **global cognition**
criterion, the **correlated** scenario led to **higher balanced
accuracy** estimates in three out of four cases. The maximum difference
was similar to the other comparison types, reaching up to 3.2%.

[TABLE]

Table 4: Expectations of balanced accuracy estimates and differences
between the independent and correlated scenarios based on independent
sample t-tests.

Lastly, Cohen’s $`\kappa`$ showed the clearest and most consistent
pattern of results ([Table 5](#tbl-kappa)). In all cases, the
**correlated** scenario yielded **higher** $`\kappa`$ values than the
independent one, with differences ranging from 0.012 to 0.068.

[TABLE]

Table 5: Expectations of Cohen’s κ estimates and differences between the
independent and correlated scenarios based on independent sample
t-tests.

## Conclusions

> Based on the simulations presented in this vignette, we can conclude
> that the correlations between FAQ and global cognition measures of the
> magnitude observed in our empirical data imply that:
>
> - the **correlated** scenario is more **sensitive** to PDD,
>   consistently yielding **higher PDD rates**,
> - the **correlated** scenario produces **accuracy estimates** that are
>   **similar** to those from the independent scenario,
> - when considering **balanced accuracy**, the **correlated** scenario
>   diverges from the independent one in more **nuanced** ways, yielding
>   **higher** estimates in some comparisons and **lower** in others,
> - relative to the independent scenario, the **correlated** scenario
>   tends to **slightly overestimate** Cohen’s $`\kappa`$ to a small
>   degree[^1], and
> - overall, the differences between the **correlated** and
>   **independent** scenarios are likely to be **small in magnitude**,
>   unless higher correlations between diagnostic criteria are
>   introduced in the data-generating process.[^2]

McHugh, Marry L. 2012. “Interrater Reliability: The Kappa Statistic.”
*Biochemia Medica*, 276–82. <https://doi.org/10.11613/bm.2012.031>.

Trizano-Hermosilla, Italo, and Jesús M. Alvarado. 2016. “Best
Alternatives to Cronbach’s Alpha Reliability in Realistic Conditions:
Congeneric and Asymmetrical Measurements.” *Frontiers in Psychology* 7
(May). <https://doi.org/10.3389/fpsyg.2016.00769>.

[^1]: Following the reference in the main text ([McHugh
    2012](#ref-mchugh2012)), a qualitative shift in the interpretation
    of Cohen’s $`\kappa`$ would require a difference of approximately
    0.2, more than twice the largest difference observed in this
    simulation study.

[^2]: Although this simulation employed a correlation matrix based on
    our empirical data, the code can be easily modified to generate data
    with stronger correlations, which would, in turn, produce larger
    differences between scenarios. For brevity, we leave this
    exploration to the reader.
