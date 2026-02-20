# Principal Component Analysis (PCA)

PCA is a multivariate data reduction technique. It summarises the data
in a smaller number of Principal Components that maximise variance.

## Usage

``` r
PCA(number_components = 2, ...)
```

## Arguments

- number_components:

  (numeric, integer) The number of Principal Components calculated. The
  default is `2`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `PCA` object with the following `output` slots:

|  |  |
|----|----|
| `scores` | (DatasetExperiment) A matrix of PCA scores where each column corresponds to a Principal Component. |
| `loadings` | (data.frame) |
| `eigenvalues` | (data.frame) |
| `ssx` | (numeric) |
| `correlation` | (data.frame) |
| `that` | (DatasetExperiment) |

## Inheritance

A `PCA` object inherits the following `struct` classes:\
\
`[PCA]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = PCA(
      number_components = 2)
```
