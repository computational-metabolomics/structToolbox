# Linear model

Linear models can be used to carry out regression, single stratum
analysis of variance and analysis of covariance.

## Usage

``` r
linear_model(formula, na_action = "na.omit", contrasts = list(), ...)
```

## Arguments

- formula:

  (formula) A symbolic description of the model to be fitted.

- na_action:

  (character) NA action. Allowed values are limited to the following:

  - `"na.omit"`: Incomplete cases are removed.

  - `"na.fail"`: An error is thrown if NA are present.

  - `"na.exclude"`: Incomplete cases are removed, and the output result
    is padded to the correct size using NA.

  - `"na.pass"`: Does not apply a linear model if NA are present.

  The default is `"na.omit"`.

- contrasts:

  (list) The contrasts associated with a factor. The default is
  [`list()`](https://rdrr.io/r/base/list.html).

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `linear_model` object with the following `output` slots:

|  |  |
|----|----|
| `lm` | (lm) The lm object for this model\_. |
| `coefficients` | (numeric) The coefficients for the fitted model\_. |
| `residuals` | (numeric) The residuals for the fitted model\_. |
| `fitted_values` | (numeric) The fitted values for the data used to train the model\_. |
| `predicted_values` | (numeric) The predicted values for new data using the fitted model\_. |
| `r_squared` | (numeric) The value of R Squared for the fitted model\_. |
| `adj_r_squared` | (numeric) The value ofAdjusted R Squared for the fitted model\_. |

## Details

This object makes use of functionality from the following packages:

- `stats`

## Inheritance

A `linear_model` object inherits the following `struct` classes:\
\
`[linear_model]` \>\> `[model]` \>\> `[struct_class]`

## References

R Core Team (2025). *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing, Vienna, Austria.
<https://www.R-project.org/>.

## Examples

``` r
M = linear_model(
      formula = y ~ x,
      na_action = "na.omit",
      contrasts = list())

D = iris_DatasetExperiment()
M = linear_model(formula = y~Species)
```
