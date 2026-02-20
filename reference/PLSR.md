# Partial least squares regression

PLS is a multivariate regression technique that extracts latent
variables maximising covariance between the input data and the response.
For regression the response is a continuous variable.

## Usage

``` r
PLSR(number_components = 2, factor_name, ...)
```

## Arguments

- number_components:

  (numeric, integer) The number of PLS components. The default is `2`.\

- factor_name:

  (character) The name of sample meta column(s) to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `PLSR` object with the following `output` slots:

|  |  |
|----|----|
| `scores` | (DatasetExperiment) |
| `loadings` | (data.frame) |
| `y` | (data.frame) |
| `yhat` | (data.frame) |
| `reg_coeff` | (data.frame) |
| `vip` | (data.frame) |
| `pls_model` | (list) |
| `sr` | (data.frame) Selectivity ratio for a variable represents a measure of a variable's importance in the PLS model. The output data.frame contains a column of selectivity ratios, a column of p-values based on an F-distribution and a column indicating significance at p \< 0.05. |
| `sr_pvalue` | (data.frame) A p-value computed from the Selectivity Ratio based on an F-distribution. |

## Details

This object makes use of functionality from the following packages:

- `pls`

## Inheritance

A `PLSR` object inherits the following `struct` classes:\
\
`[PLSR]` \>\> `[model]` \>\> `[struct_class]`

## References

Liland K, Mevik B, Wehrens R (2024). *pls: Partial Least Squares and
Principal Component Regression*. doi:10.32614/CRAN.package.pls
<https://doi.org/10.32614/CRAN.package.pls>, R package version 2.8-5,
<https://CRAN.R-project.org/package=pls>.

## Examples

``` r
M = PLSR(
      number_components = 2,
      factor_name = "V1")

M = PLSR(factor_name='run_order')
```
