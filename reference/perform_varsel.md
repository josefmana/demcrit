# Perform variable selection

A wrapper for
[`projpred::cv_varsel()`](https://mc-stan.org/projpred/reference/cv_varsel.html).
Using a provided reference model, runs a projective prediction feature
selection via cross-validation.

## Usage

``` r
perform_varsel(refm_fit, ...)
```

## Arguments

- refm_fit:

  Reference model, e.g., fitted via `fit_reference`.

- ...:

  Parameters passed to
  [`projpred::cv_varsel()`](https://mc-stan.org/projpred/reference/cv_varsel.html).

## Value

An object of class vsel as returned by
[`projpred::cv_varsel()`](https://mc-stan.org/projpred/reference/cv_varsel.html).

## See also

- [`fit_reference()`](https://josefmana.github.io/demcrit/reference/fit_reference.md)

- [`projpred::cv_varsel()`](https://mc-stan.org/projpred/reference/cv_varsel.html)
