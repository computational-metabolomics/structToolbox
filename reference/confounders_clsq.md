# Check for confounding factors

Univariate least squares regression models are used to compare models
with and without potential confounding factors included. The change in
coefficients (delta) is then computed for each potential confounding
factor. Factors with a large delta are said to be having a large impact
on the model and are therefore confounding. p-values are computed for
models with confounders included to reduce potential false positives.
Only suitable for main factors with 2 levels.

## Usage

``` r
confounders_clsq(
  alpha = 0.05,
  mtc = "fdr",
  factor_name,
  confounding_factors,
  threshold = 0.15,
  ...
)
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

  (character) The name of the main factor with which other factors may
  be confounding.

- confounding_factors:

  (character) The name(s) of factor(s) that are potential confounding
  factors.

- threshold:

  (numeric) Factors with a delta greater than the the threshold are
  considered to be confounding. The default is `0.15`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `confounders_clsq` object with the following `output` slots:

|                         |              |
|-------------------------|--------------|
| `coefficients`          | (data.frame) |
| `p_value`               | (data.frame) |
| `significant`           | (data.frame) |
| `percent_change`        | (data.frame) |
| `potential_confounders` | (list)       |

## Inheritance

A `confounders_clsq` object inherits the following `struct` classes:\
\
`[confounders_clsq]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = confounders_clsq(
      alpha = 0.05,
      mtc = "fdr",
      factor_name = character(0),
      confounding_factors = character(0),
      threshold = 0.15)

D = MTBLS79_DatasetExperiment()
M = filter_by_name(mode='include',dimension='variable',
        names=colnames(D$data)[1:10]) + # first 10 features
    filter_smeta(mode='exclude',levels='QC',
        factor_name='Class') + # reduce to two group comparison
    confounders_clsq(factor_name = 'Class',
        confounding_factors=c('run_order','Batch'))
M = model_apply(M,D)
```
