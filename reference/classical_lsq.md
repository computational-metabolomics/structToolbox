# Univariate Classical Least Squares Regression

In univariate classical least squares regression a line is fitted
between each feature/variable and a response variable. The fitted line
minimises the sum of squared differences between the true response and
the predicted response. The coefficients (offset, gradient) of the fit
can be tested for significance.

## Usage

``` r
classical_lsq(alpha = 0.05, mtc = "fdr", factor_names, intercept = TRUE, ...)
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

  (character, list) The column names to regress against. If a character
  vector then the same list is used ofr all features. If a list of
  character vectors is provided it is assumed there is a different set
  of columns for each feature.

- intercept:

  (logical) Model intercept. Allowed values are limited to the
  following:

  - `"TRUE"`: An intercept term is included in the model.

  - `"FALSE"`: An intercept term is not included in the model.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `classical_lsq` object with the following `output` slots:

|  |  |
|----|----|
| `coefficients` | (data.frame) The regression coefficients for each term in the model. |
| `p_value` | (data.frame) The probability of observing the calculated statistic if the null hypothesis is true. |
| `significant` | (data.frame) True/False indicating whether the p-value computed for each variable is less than the threshold. |
| `r_squared` | (data.frame) The value of R Squared for the fitted model. |
| `adj_r_squared` | (data.frame) The value ofAdjusted R Squared for the fitted model. |

## Inheritance

A `classical_lsq` object inherits the following `struct` classes:\
\
`[classical_lsq]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = classical_lsq(
      alpha = 0.05,
      mtc = "fdr",
      factor_names = "V1",
      intercept = FALSE)

D = iris_DatasetExperiment()
M = classical_lsq(factor_names = 'Species')
M = model_apply(M,D)
```
