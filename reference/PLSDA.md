# Partial least squares discriminant analysis

PLS is a multivariate regression technique that extracts latent
variables maximising covariance between the input data and the response.
The Discriminant Analysis variant uses group labels in the response
variable. For \>2 groups a 1-vs-all approach is used. Group membership
can be predicted for test samples based on a probability estimate of
group membership, or the estimated y-value.

## Usage

``` r
PLSDA(number_components = 2, factor_name, pred_method = "max_prob", ...)
```

## Arguments

- number_components:

  (numeric, integer) The number of PLS components. The default is `2`.\

- factor_name:

  (character) The name of a sample-meta column to use.

- pred_method:

  (character) Prediction method. Allowed values are limited to the
  following:

  - `"max_yhat"`: The predicted group is selected based on the largest
    value of y_hat.

  - `"max_prob"`: The predicted group is selected based on the largest
    probability of group membership.

  The default is `"max_prob"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `PLSDA` object with the following `output` slots:

|  |  |
|----|----|
| `scores` | (DatasetExperiment) |
| `loadings` | (data.frame) |
| `yhat` | (data.frame) |
| `design_matrix` | (data.frame) |
| `y` | (data.frame) |
| `reg_coeff` | (data.frame) |
| `probability` | (data.frame) |
| `vip` | (data.frame) |
| `pls_model` | (list) |
| `sr` | (data.frame) Selectivity ratio for a variable represents a measure of a variable's importance in the PLS model. The output data.frame contains a column of selectivity ratios, a column of p-values based on an F-distribution and a column indicating significance at p \< 0.05. |
| `sr_pvalue` | (data.frame) A p-value computed from the Selectivity Ratio based on an F-distribution. |
| `predicted_labels` | (data.frame) Predicted label(s) for the input samples. |
| `prob_model` | (data.frame) |

## Details

This object makes use of functionality from the following packages:

- `pls`

## Inheritance

A `PLSDA` object inherits the following `struct` classes:\
\
`[PLSDA]` \>\> `[PLSR]` \>\> `[model]` \>\> `[struct_class]`

## References

Liland K, Mevik B, Wehrens R (2024). *pls: Partial Least Squares and
Principal Component Regression*. doi:10.32614/CRAN.package.pls
<https://doi.org/10.32614/CRAN.package.pls>, R package version 2.8-5,
<https://CRAN.R-project.org/package=pls>.

Perez NF, Ferre J, Boque R (2009). "Calculation of the reliability of
classification in discriminant partial least-squares binary
classification." *Chemometrics and Intelligent Laboratory Systems*,
*95*(2), 122-128.

Barker M, Rayens W (2003). "Partial least squares for discrimination."
*Journal of Chemometrics*, *17*(3), 166-173.

## Examples

``` r
M = PLSDA(
      number_components = 2,
      factor_name = "V1",
      pred_method = "max_prob")

M = PLSDA('number_components'=2,factor_name='Species')
```
