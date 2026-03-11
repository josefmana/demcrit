# Compute Regression z-scores

Computes z-scores for a given test index using regression parameters
from a normative calculator, along with the patient's raw score and
demographic information (age, gender, education).

## Usage

``` r
compute_z_score(calc, x, lbl, AGE, GEN, EDU)
```

## Arguments

- calc:

  A data frame or tibble containing regression parameters from the
  calculator.

- x:

  A numeric vector of raw performance scores.

- lbl:

  A character string specifying the test index label (must match an
  entry in `calc`).

- AGE:

  A numeric vector indicating the participant's age (in years).

- GEN:

  A numeric vector indicating gender (1 = man, 0 = woman).

- EDU:

  A numeric vector indicating years of education.

## Value

A numeric vector of computed z-scores, one for each raw score in `x`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
z <- compute_z_score(
  calc = regression_table,
  x = c(35, 42, 28),
  lbl = "Digit Span Backward",
  AGE = c(67, 70, 74),
  GEN = c(1, 0, 1),
  EDU = c(15, 12, 16)
)
} # }
```
