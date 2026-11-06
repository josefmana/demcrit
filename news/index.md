# Changelog

## demcrit 2.0.0

### Corrections to Mana et al. (2026)

Two errors were found after publication of the article. Both were
reported by @elliottower. None of the conclusions of the article change.
See
[`vignette("corrections")`](https://josefmana.github.io/demcrit/articles/corrections.md)
for every affected result.

- MoCA-based algorithms now apply the global criterion MoCA \< 27, as
  stated in Table 1 of the article. Published results used MoCA \< 26
  (#21). 12 of 68 algorithms (MoCA-based, IADL deficit by FAQ total
  score) gain 1 to 2 cases of probable PDD. Mean PDD rate changes from
  6.21% to 6.34% (FAQ total algorithms: 9.10% to 9.37%); medians, ranges
  and all FAQ item 9 algorithms are unchanged.

- `concords` is now computed on the 190 complete cases described in the
  article, consistent with `rates`. Published values used all cases
  available for each pair of algorithms, N = 190 to 202 (#20). Cohen’s
  kappa among FAQ total algorithms changes from 0.75 to 0.74, between
  algorithms differing only in IADL definition from 0.43 to 0.42; sMoCA
  sensitivity for Level II (FAQ total) classification changes from 0.95
  to 1.00.

### New data

- `rates_published` and `concords_published` contain the values printed
  in the article (demcrit v1.2.0), kept for comparison.

### Other

- New vignette
  [`vignette("corrections")`](https://josefmana.github.io/demcrit/articles/corrections.md).
- The manuscript source (`_manuscript/manuscript.qmd`) now reads the
  current targets and renders the corrected author version of the
  article.
- Package suggestions needed by the targets pipeline are listed in
  `DESCRIPTION` (#17).
