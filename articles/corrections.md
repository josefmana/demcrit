# Corrections to Mana et al. (2026)

> This article documents two errors found after publication of Mana et
> al. ([2026](https://doi.org/10.1177/1877718X261450361)) and lists
> every published result affected by their correction. Both errors were
> reported by [elliottower](https://github.com/elliottower) in issues
> [\#20](https://github.com/josefmana/demcrit/issues/20) and
> [\#21](https://github.com/josefmana/demcrit/issues/21). The
> corrections were made by Jožka Mana and have not been reviewed by the
> other authors or by the journal. **None of the conclusions of the
> article change.**

## What was wrong

1.  **MoCA threshold.** Table 1 of the article states that MoCA-based
    algorithms define global cognitive deficit as MoCA \< 27. The code
    applied MoCA \< 26, the same value as was stated in the Appendix to
    the article. The code now applies MoCA \< 27, as stated in the
    article.

2.  **Missing data in the concordance analysis.** The Methods state that
    analyses were done on complete cases (N = 190). The concordance
    statistics (Cohen’s κ, accuracy, sensitivity, specificity) were
    instead computed on all cases available for each pair of algorithms
    (N = 190 to 202). They are now computed on the 190 complete cases,
    as stated in the article.

The values printed in the article are distributed with the package as
`rates_published` and `concords_published`. Version 1.2.0 of the package
reproduces the article as printed:

``` r

devtools::install_github("josefmana/demcrit@v1.2.0")
```

## Changed statements

| Location | Quantity | Published | Corrected |
|:---|:---|:---|:---|
| Abstract; Results (Concordance between algorithms) | Cohen’s κ, same IADL definition (FAQ total) | 0.75, SD = 0.14 | 0.74, SD = 0.13 |
| Abstract; Results (Concordance between algorithms) | Cohen’s κ, IADL definitions differ | 0.43, SD = 0.08 | 0.42, SD = 0.08 |
| Results (Dementia rate estimates) | Mean PDD rate, all algorithms | 6.21% (SD = 3.43) | 6.34% (SD = 3.63) |
| Results (Dementia rate estimates) | Mean PDD rate, FAQ total algorithms | 9.10% (SD = 2.53) | 9.37% (SD = 2.77) |
| Results (Descriptive analysis) | sMoCA sensitivity for Level II (FAQ total) | 0.95 | 1.00 |

Table 1

In addition, the following tables and figures change accordingly: Table
A3 (rates per algorithm), Tables A4 and A5 (best and worst
approximations of Level II), Figure 1 (dot sizes and bold edges), Figure
2 and Figures A7 to A9 (concordance matrices), and Figures A4 to A6
(rates and their demographic predictors).

## What does not change

- The range and median of PDD rate estimates (2.11% to 16.84%, Md =
  4.21%), and all algorithms using FAQ item 9 to define IADL deficit.
- Cohen’s κ among algorithms using FAQ item 9 (0.86, SD = 0.09).
- All MMSE-based, sMoCA-based and Level II algorithms.
- The exploratory variable selection, the screening model (MoCA Five
  words and MMSE Sevens), Table 3 and Figure 3, which do not depend on
  the MoCA total score.
- The verbal conclusions: top-ranked Level I algorithms are MoCA-based
  and mostly use Clock drawing and Animal naming; sMoCA achieves the
  highest balanced accuracy for Level II (FAQ total) classification.

## Changed PDD rates

The corrected threshold changes the classification of 12 of 68
algorithms, all MoCA-based with IADL deficit defined by FAQ total score.

| Algorithm | Executive | Memory | Language | Rate (published) | Rate (corrected) |
|:---|:---|:---|:---|:---|:---|
| MoCA (53) | Clock drawing \< 3 | Five words \< 4 | Abstraction \< 2 | 23 (12.11%) | 25 (13.16%) |
| MoCA (57) | Clock drawing \< 3 | Five words \< 5 | Abstraction \< 2 | 23 (12.11%) | 25 (13.16%) |
| MoCA (49) | Clock drawing \< 3 | Five words \< 3 | Abstraction \< 2 | 22 (11.58%) | 24 (12.63%) |
| MoCA (45) | Clock drawing \< 3 | Five words \< 2 | Abstraction \< 2 | 22 (11.58%) | 23 (12.11%) |
| MoCA (59) | Clock drawing \< 3 | Five words \< 5 | Animal naming \< 3 | 21 (11.05%) | 23 (12.11%) |
| MoCA (33) | VF K \< 11 | Five words \< 4 | Abstraction \< 2 | 21 (11.05%) | 22 (11.58%) |
| MoCA (37) | VF K \< 11 | Five words \< 5 | Abstraction \< 2 | 21 (11.05%) | 22 (11.58%) |
| MoCA (55) | Clock drawing \< 3 | Five words \< 4 | Animal naming \< 3 | 20 (10.53%) | 22 (11.58%) |
| MoCA (13) | Clock drawing \< 2 | Five words \< 4 | Abstraction \< 2 | 20 (10.53%) | 21 (11.05%) |
| MoCA (17) | Clock drawing \< 2 | Five words \< 5 | Abstraction \< 2 | 20 (10.53%) | 21 (11.05%) |
| MoCA (41) | Clock drawing \< 3 | Five words \< 1 | Abstraction \< 2 | 19 (10.00%) | 20 (10.53%) |
| MoCA (51) | Clock drawing \< 3 | Five words \< 3 | Animal naming \< 3 | 19 (10.00%) | 20 (10.53%) |

Table 2

## Corrected Tables A4 and A5

- [Table A4 (accuracy), corrected](#tabset-1-1)
- [Table A4, published](#tabset-1-2)
- [Table A5 (balanced accuracy), corrected](#tabset-1-3)
- [Table A5, published](#tabset-1-4)

&nbsp;

- [TABLE]

  Table 3

[TABLE]

Table 4

[TABLE]

Table 5

[TABLE]

Table 6

## Corrected manuscript

A corrected author version of the manuscript, generated from
`_manuscript/manuscript.qmd` in this repository, is attached to the
[v2.0.0
release](https://github.com/josefmana/demcrit/releases/tag/v2.0.0).
