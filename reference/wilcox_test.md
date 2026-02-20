# wilcoxon signed rank test

A Mann-Whitney-Wilcoxon signed rank test compares ,the ranks of values
in two groups. It is the non-parametric equivalent of a t-test. Multiple
test corrected p-values are computed as indicators of significance for
each variable/feature.

## Usage

``` r
wilcox_test(
  alpha = 0.05,
  mtc = "fdr",
  factor_names,
  paired = FALSE,
  paired_factor = character(0),
  conf_level = 0.95,
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

  (character) The name of a sample-meta column to use.

- paired:

  (logical) Apply a paired test. The default is `FALSE`.\

- paired_factor:

  (character) The factor name containing sample ids for paired data. The
  default is `character(0)`.

- conf_level:

  (numeric) The confidence level of the interval. The default is
  `0.95`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `wilcox_test` object with the following `output` slots:

|  |  |
|----|----|
| `statistic` | (data.frame) The value of the calculated statistic which is converted to a p-value. |
| `p_value` | (data.frame) The probability of observing the calculated t-statistic. |
| `dof` | (numeric) The number of degrees of freedom used to calculate the test statistic. |
| `significant` | (data.frame) TRUE if the calculated p-value is less than the supplied threhold (alpha). |
| `conf_int` | (data.frame) Confidence interval for t statistic. |
| `estimates` | (data.frame) The group estimates used when computing the statistic. |

struct object

## Inheritance

A `wilcox_test` object inherits the following `struct` classes:\
\
`[wilcox_test]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = wilcox_test(
      alpha = 0.05,
      mtc = "fdr",
      factor_names = "V1",
      paired = FALSE,
      paired_factor = character(0),
      conf_level = 0.95)

M = wilcox_test(factor_name='Class')
```
