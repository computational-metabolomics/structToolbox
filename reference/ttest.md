# t-test

A t-test compares the means of two factor levels. Multiple-test
corrected p-values are used to indicate the significance of the computed
difference for all features.

## Usage

``` r
ttest(
  alpha = 0.05,
  mtc = "fdr",
  factor_names,
  paired = FALSE,
  paired_factor = character(0),
  equal_variance = FALSE,
  conf_level = 0.95,
  control_group = NULL,
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

- factor_names:

  (character) The name of sample meta column(s) to use.

- paired:

  (logical) Apply a paired t-test. The default is `FALSE`.\

- paired_factor:

  (character) The factor name that encodes the sample id for pairing.
  The default is `character(0)`.

- equal_variance:

  (logical) Equal variance. Allowed values are limited to the following:

  - `"TRUE"`: The variance of each group is treated as being equal using
    the pooled variance to estimate the variance.

  - `"FALSE"`: The variance of each group is not assumed to be equal and
    the Welch (or Satterthwaite) approximation is used.

  The default is `FALSE`.\

- conf_level:

  (numeric) The confidence level of the interval. The default is
  `0.95`.\

- control_group:

  (character, NULL) The level name of the group used as the second group
  (where possible) when computing t-statistics. This ensures a positive
  t-statistic corresponds to an increase when compared to the control
  group. The default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `ttest` object with the following `output` slots:

|  |  |
|----|----|
| `t_statistic` | (data.frame) The value of the calculate statistics which is converted to a p-value when compared to a t-distribution. |
| `p_value` | (data.frame) The probability of observing the calculated t-statistic. |
| `dof` | (numeric) The number of degrees of freedom used to calculate the test statistic. |
| `significant` | (data.frame) TRUE if the calculated p-value is less than the supplied threhold (alpha). |
| `conf_int` | (data.frame) Confidence interval for t statistic. |
| `estimates` | (data.frame) The group means estimated when computing the t-statistic. |

## Inheritance

A `ttest` object inherits the following `struct` classes:\
\
`[ttest]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = ttest(
      alpha = 0.05,
      mtc = "fdr",
      factor_names = "V1",
      paired = FALSE,
      paired_factor = "NA",
      equal_variance = FALSE,
      conf_level = 0.95,
      control_group = NULL)

M = ttest(factor_name='Class')
```
