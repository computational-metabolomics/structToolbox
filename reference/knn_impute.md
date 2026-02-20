# kNN missing value imputation

k-nearest neighbour missing value imputation replaces missing values in
the data with the average of a predefined number of the most similar
neighbours for which the value is present

## Usage

``` r
knn_impute(
  neighbours = 5,
  sample_max = 50,
  feature_max = 50,
  by = "features",
  ...
)
```

## Arguments

- neighbours:

  (numeric) The number of neighbours (k) to use for imputation. The
  default is `5`.\

- sample_max:

  (numeric) The maximum percent missing values per sample. The default
  is `50`.\

- feature_max:

  (numeric) The maximum percent missing values per feature. The default
  is `50`.\

- by:

  (character) Impute using similar "samples" or "features". Default
  features. The default is `"features"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `knn_impute` object with the following `output` slots:

|  |  |
|----|----|
| `imputed` | (DatasetExperiment) A DatasetExperiment object containing the data where missing values have been imputed. |

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `knn_impute` object inherits the following `struct` classes:\
\
`[knn_impute]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

## Examples

``` r
M = knn_impute(
      neighbours = 5,
      feature_max = 50,
      sample_max = 50,
      by = "features")

M = knn_impute()
```
