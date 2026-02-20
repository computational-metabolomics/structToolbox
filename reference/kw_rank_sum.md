# Kruskal-Wallis rank sum test

The Kruskal-Wallis test is a univariate hypothesis testing method that
allows multiple (n\>=2) groups to be compared without making the
assumption that values are normally distributed. It is the
non-parametric equivalent of a 1-way ANOVA. The test is applied to all
variables/features individually, and multiple test corrected p-values
are computed to indicate the significance of variables/features.

## Usage

``` r
kw_rank_sum(alpha = 0.05, mtc = "fdr", factor_names, ...)
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

- factor_names:

  (character) The name of sample meta column(s) to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `kw_rank_sum` object with the following `output` slots:

|  |  |
|----|----|
| `test_statistic` | (data.frame) The value of the calculated statistic which is converted to a p-value when compared to a chi2-distribution. |
| `p_value` | (data.frame) The probability of observing the calculated statistic. |
| `dof` | (numeric) The number of degrees of freedom used to calculate the test statistic. |
| `significant` | (data.frame) TRUE if the calculated p-value is less than the supplied threhold (alpha). |
| `estimates` | (data.frame) |

## Inheritance

A `kw_rank_sum` object inherits the following `struct` classes:\
\
`[kw_rank_sum]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = kw_rank_sum(
      alpha = 0.05,
      mtc = "fdr",
      factor_names = "V1")

D = iris_DatasetExperiment()
M = kw_rank_sum(factor_names='Species')
M = model_apply(M,D)
```
