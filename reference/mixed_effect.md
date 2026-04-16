# Mixed effects model

A mixed effects model is an extension of ANOVA where there are both
fixed and random effects.

## Usage

``` r
mixed_effect(alpha = 0.05, mtc = "fdr", formula, ss_type = "marginal", ...)
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

- ss_type:

  (character) Sum of squares type. Allowed values are limited to the
  following:

  - `"marginal"`: Type III sum of squares.

  - `"sequential"`: Type II sum of squares.

  The default is `"marginal"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `mixed_effect` object with the following `output` slots:

|  |  |
|----|----|
| `f_statistic` | (data.frame) The value of the calculated statistic. |
| `p_value` | (data.frame) The probability of observing the calculated statistic if the null hypothesis is true. |
| `significant` | (data.frame) True/False indicating whether the p-value computed for each variable is less than the threshold. |

## Details

This object makes use of functionality from the following packages:

- `nlme`

- `emmeans`

## Inheritance

A `mixed_effect` object inherits the following `struct` classes:\
\
`[mixed_effect]` \>\> `[ANOVA]` \>\> `[model]` \>\> `[struct_class]`

## References

Pinheiro J, Bates D, R Core Team (2025). *nlme: Linear and Nonlinear
Mixed Effects Models*. doi:10.32614/CRAN.package.nlme
<https://doi.org/10.32614/CRAN.package.nlme>, R package version 3.1-168,
<https://CRAN.R-project.org/package=nlme>.

Pinheiro JC, Bates DM (2000). *Mixed-Effects Models in S and S-PLUS*.
Springer, New York. doi:10.1007/b98882 <https://doi.org/10.1007/b98882>.

Lenth R, Piaskowski J (2025). *emmeans: Estimated Marginal Means, aka
Least-Squares Means*. doi:10.32614/CRAN.package.emmeans
<https://doi.org/10.32614/CRAN.package.emmeans>, R package version
2.0.1, <https://CRAN.R-project.org/package=emmeans>.

Fox J, Weisberg S (2019). *An R Companion to Applied Regression*, Third
edition. Sage, Thousand Oaks CA. <https://www.john-fox.ca/Companion/>.

## Examples

``` r
M = mixed_effect(
      alpha = 0.05,
      mtc = "fdr",
      formula = y ~ x,
      ss_type = "marginal")

D = iris_DatasetExperiment()
D$sample_meta$id=rownames(D) # dummy id column
M = mixed_effect(formula = y~Species+ Error(id/Species))
M = model_apply(M,D)
```
