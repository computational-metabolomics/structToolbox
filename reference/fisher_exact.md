# Fisher Exact Test

A fisher exact test is used to analyse contingency tables by comparing
the number of correctly/incorrectly predicted group labels. A multiple
test corrected p-value indicates whether the number of measured values
is significantly different between groups.

## Usage

``` r
fisher_exact(alpha = 0.05, mtc = "fdr", factor_name, factor_pred, ...)
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

- factor_name:

  (character) The name of a sample-meta column to use.

- factor_pred:

  (data.frame) A data.frame, where each column is a factor of predicted
  group labels to compare with the true groups labels.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `fisher_exact` object with the following `output` slots:

|  |  |
|----|----|
| `p_value` | (data.frame) The probability of observing the calculated statistic if the null hypothesis is true. |
| `significant` | (data.frame) True/False indicating whether the p-value computed for each variable is less than the threshold. |

## Inheritance

A `fisher_exact` object inherits the following `struct` classes:\
\
`[fisher_exact]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = fisher_exact(
      alpha = 0.05,
      mtc = "fdr",
      factor_name = "V1",
      factor_pred = data.frame(id=NA))

# load some data
D=MTBLS79_DatasetExperiment()

# prepare predictions based on NA
pred=as.data.frame(is.na(D$data))
pred=lapply(pred,factor,levels=c(TRUE,FALSE))
pred=as.data.frame(pred)

# apply method
M = fisher_exact(alpha=0.05,mtc='fdr',factor_name='Class',factor_pred=pred)
M=model_apply(M,D)
```
