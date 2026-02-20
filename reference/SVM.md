# Support Vector Machine Classifier

Support Vector Machines (SVM) are a machine learning algorithm for
classification. They can make use of kernel functions to generate highly
non-linear boundaries between groups.

## Usage

``` r
SVM(
  factor_name,
  kernel = "linear",
  degree = 3,
  gamma = 1,
  coef0 = 0,
  cost = 1,
  class_weights = NULL,
  ...
)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- kernel:

  (character) Kernel type. Allowed values are limited to the following:

  - `"linear"`: .

  - `"polynomial"`: .

  - `"radial"`: .

  - `"sigmoid"`: .

  The default is `"linear"`.

- degree:

  (numeric) The polynomial degree. The default is `3`.\

- gamma:

  (numeric) The gamma parameter. The default is `1`.\

- coef0:

  (numeric) The offset coefficient. The default is `0`.\

- cost:

  (numeric) The cost of violating the constraints. The default is `1`.\

- class_weights:

  (numeric, character, NULL) A named vector of weights for the different
  classes. Specifying "inverse" will choose the weights inversely
  proportional to the class distribution. The default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `SVM` object with the following `output` slots:

|                   |              |
|-------------------|--------------|
| `SV`              | (matrix)     |
| `index`           | (numeric)    |
| `coefs`           | (matrix)     |
| `pred`            | (data.frame) |
| `decision_values` | (data.frame) |

struct object

## Details

This object makes use of functionality from the following packages:

- `e1071`

## Inheritance

A `SVM` object inherits the following `struct` classes:\
\
`[SVM]` \>\> `[model]` \>\> `[struct_class]`

## References

Meyer D, Dimitriadou E, Hornik K, Weingessel A, Leisch F (2025). *e1071:
Misc Functions of the Department of Statistics, Probability Theory Group
(Formerly: E1071), TU Wien*. doi:10.32614/CRAN.package.e1071
<https://doi.org/10.32614/CRAN.package.e1071>, R package version 1.7-17,
<https://CRAN.R-project.org/package=e1071>.

Brereton RG, Lloyd GR (2010). "Support Vector Machines for
classification and regression." *The Analyst*, *135*(2), 230-267.

## Examples

``` r
M = SVM(
      factor_name = "V1",
      kernel = "linear",
      degree = 3,
      gamma = 1,
      coef0 = 0,
      cost = 1,
      class_weights = 1)

M = SVM(factor_name='Species',gamma=1)
```
