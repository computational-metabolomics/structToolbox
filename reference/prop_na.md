# Fisher's exact test for missing values

A Fisher's exact test is used to compare the number of missing values in
each group. Multiple test corrected p-values are computed to indicate
whether there is a significant difference in the number of missing
values across groups for each feature.

## Usage

``` r
prop_na(alpha = 0.05, mtc = "fdr", factor_name, ...)
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

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `prop_na` object with the following `output` slots:

|  |  |
|----|----|
| `p_value` | (data.frame) The probability of observing the calculated statistic. |
| `significant` | (data.frame) TRUE if the calculated p-value is less than the supplied threshold (alpha). |
| `na_count` | (data.frame) The number of NA values per group of the chosen factor. |

struct object

## Inheritance

A `prop_na` object inherits the following `struct` classes:\
\
`[prop_na]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = prop_na(
      alpha = 0.05,
      mtc = "fdr",
      factor_name = "V1")

M = prop_na(factor_name='Species')
```
