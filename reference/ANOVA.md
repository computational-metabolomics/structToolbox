# Analysis of Variance

Analysis of Variance (ANOVA) is a univariate method used to analyse the
difference among group means. Multiple test corrected p-values are
computed to indicate significance for each feature.

## Usage

``` r
ANOVA(alpha = 0.05, mtc = "fdr", formula, ss_type = "III", ...)
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

  (character) ANOVA sum of squares. Allowed values are limited to the
  following:

  - `"I"`: Type I sum of squares.

  - `"II"`: Type II sum of squares.

  - `"III"`: Type III sum of squares.

  The default is `"III"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `ANOVA` object with the following `output` slots:

|  |  |
|----|----|
| `f_statistic` | (data.frame) The value of the calculated statistic. |
| `p_value` | (data.frame) The probability of observing the calculated statistic if the null hypothesis is true. |
| `significant` | (data.frame) True/False indicating whether the p-value computed for each variable is less than the threshold. |

## Details

This object makes use of functionality from the following packages:

- `car`

## Inheritance

A `ANOVA` object inherits the following `struct` classes:\
\
`[ANOVA]` \>\> `[model]` \>\> `[struct_class]`

## References

Fox J, Weisberg S (2019). *An R Companion to Applied Regression*, Third
edition. Sage, Thousand Oaks CA. <https://www.john-fox.ca/Companion/>.

## Examples

``` r
M = ANOVA(
      alpha = 0.05,
      mtc = "fdr",
      formula = y ~ x,
      ss_type = "III")

D = iris_DatasetExperiment()
M = ANOVA(formula=y~Species)
M = model_apply(M,D)
```
