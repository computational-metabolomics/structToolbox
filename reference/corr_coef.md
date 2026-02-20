# Correlation coefficient

The correlation between features and a set of continuous factor are
calculated. Multiple-test corrected p-values are used to indicate
whether the computed coefficients may have occurred by chance.

## Usage

``` r
corr_coef(alpha = 0.05, mtc = "fdr", factor_names, method = "spearman", ...)
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

- method:

  (character) Type of correlation. Allowed values are limited to the
  following:

  - `"kendall"`: Kendall's tau is computed.

  - `"pearson"`: Pearson product moment correlation is computed.

  - `"spearman"`: Spearman's rho statistic is computed.

  The default is `"spearman"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `corr_coef` object with the following `output` slots:

|  |  |
|----|----|
| `coeff` | (data.frame) The value of the calculate statistics which is converted to a p-value when compared to a t-distribution. |
| `p_value` | (data.frame) The probability of observing the calculated statistic if the null hypothesis is true. |
| `significant` | (data.frame) True/False indicating whether the p-value computed for each variable is less than the threshold. |

## Details

This object makes use of functionality from the following packages:

- `stats`

## Inheritance

A `corr_coef` object inherits the following `struct` classes:\
\
`[corr_coef]` \>\> `[model]` \>\> `[struct_class]`

## References

R Core Team (2025). *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing, Vienna, Austria.
<https://www.R-project.org/>.

## Examples

``` r
M = corr_coef(
      alpha = 0.05,
      mtc = "fdr",
      factor_names = "V1",
      method = "spearman")

D = MTBLS79_DatasetExperiment(filtered=TRUE)

# subset for this example
D = D[,1:10]

# convert to numeric for this example
D$sample_meta$sample_order=as.numeric(D$sample_meta$run_order)
D$sample_meta$sample_rep=as.numeric(D$sample_meta$Sample_Rep)

M = corr_coef(factor_names=c('sample_order','sample_rep'))
M = model_apply(M,D)
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
```
