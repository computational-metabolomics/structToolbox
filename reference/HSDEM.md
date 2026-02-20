# Tukey's Honest Significant Difference using estimated marginal means

Tukey's HSD post hoc test is a modified t-test applied for all features
to all pairs of levels in a factor. It is used to determine which groups
are different (if any). A multiple test corrected p-value is computed to
indicate which groups are significantly different to the others for each
feature. For mixed effects models estimated marginal means are used.

## Usage

``` r
HSDEM(alpha = 0.05, mtc = "fdr", formula, ...)
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

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `HSDEM` object with the following `output` slots:

|  |  |
|----|----|
| `p_value` | (data.frame) The probability of observing the calculated statistic if the null hypothesis is true. |
| `significant` | (data.frame) True/False indicating whether the p-value computed for each variable is less than the threshold. |

## Details

This object makes use of functionality from the following packages:

- `emmeans`

- `nlme`

## Inheritance

A `HSDEM` object inherits the following `struct` classes:\
\
`[HSDEM]` \>\> `[model]` \>\> `[struct_class]`

## References

Lenth R, Piaskowski J (2025). *emmeans: Estimated Marginal Means, aka
Least-Squares Means*. doi:10.32614/CRAN.package.emmeans
<https://doi.org/10.32614/CRAN.package.emmeans>, R package version
2.0.1, <https://CRAN.R-project.org/package=emmeans>.

Pinheiro J, Bates D, R Core Team (2025). *nlme: Linear and Nonlinear
Mixed Effects Models*. doi:10.32614/CRAN.package.nlme
<https://doi.org/10.32614/CRAN.package.nlme>, R package version 3.1-168,
<https://CRAN.R-project.org/package=nlme>.

Pinheiro JC, Bates DM (2000). *Mixed-Effects Models in S and S-PLUS*.
Springer, New York. doi:10.1007/b98882 <https://doi.org/10.1007/b98882>.

## Examples

``` r
M = HSDEM(
      alpha = 0.05,
      mtc = "fdr",
      formula = y ~ x)

D = iris_DatasetExperiment()
D$sample_meta$id=rownames(D) # dummy id column
M = HSDEM(formula = y~Species+ Error(id/Species))
M = model_apply(M,D)
```
