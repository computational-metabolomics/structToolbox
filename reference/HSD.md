# Tukey's Honest Significant Difference

Tukey's HSD post hoc test is a modified t-test applied for all features
to all pairs of levels in a factor. It is used to determine which groups
are different (if any). A multiple test corrected p-value is computed to
indicate which groups are significantly different to the others for each
feature.

## Usage

``` r
HSD(alpha = 0.05, mtc = "fdr", formula, unbalanced = FALSE, ...)
```

## Arguments

- alpha:

  (numeric) The p-value cutoff for determining significance. The default
  is `0.05`.\

- mtc:

  (character) Multiple test correction method. Allowed values are
  limited to the following:

  - `"bonferroni"`: Bonferroni correction in which the p-values are
    multiplied by the number of comparisons.

  - `"fdr"`: Benjamini and Hochberg False Discovery Rate correction.

  - `"none"`: No correction.

  The default is `"fdr"`.

- formula:

  (formula) A symbolic description of the model to be fitted.

- unbalanced:

  (logical) Unbalanced model. Allowed values are limited to the
  following:

  - `"TRUE"`: A correction is applied for unbalanced designs.

  - `"FALSE"`: No correction is applied for unbalanced designs.

  The default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `HSD` object with the following `output` slots:

|  |  |
|----|----|
| `difference` | (data.frame) |
| `UCL` | (data.frame) |
| `LCL` | (data.frame) |
| `p_value` | (data.frame) The probability of observing the calculated statistic if the null hypothesis is true. |
| `significant` | (data.frame) True/False indicating whether the p-value computed for each variable is less than the threshold. |

## Details

This object makes use of functionality from the following packages:

- `agricolae`

## Inheritance

A `HSD` object inherits the following `struct` classes:\
\
`[HSD]` \>\> `[model]` \>\> `[struct_class]`

## References

de Mendiburu F (2023). *agricolae: Statistical Procedures for
Agricultural Research*. doi:10.32614/CRAN.package.agricolae
<https://doi.org/10.32614/CRAN.package.agricolae>, R package version
1.3-7, <https://CRAN.R-project.org/package=agricolae>.

## Examples

``` r
M = HSD(
      alpha = 0.05,
      mtc = "fdr",
      formula = y ~ x,
      unbalanced = FALSE)

D = iris_DatasetExperiment()
M = HSD(formula=y~Species)
M = model_apply(M,D)
```
