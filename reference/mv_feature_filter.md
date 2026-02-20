# Filter features by missing values

Removes features where the percentage of non-missing values falls below
a threshold.

## Usage

``` r
mv_feature_filter(
  threshold = 20,
  qc_label = "QC",
  method = "QC",
  factor_name,
  ...
)
```

## Arguments

- threshold:

  (numeric) The minimum percentage of non-missing values. The default is
  `20`.\

- qc_label:

  (character) The label used to identify QC/group samples when using the
  "QC" (within a named group) filtering method. The default is `"QC"`.

- method:

  (character) Filtering method. Allowed values are limited to the
  following:

  - `"within_all"`: Features are removed if the threshold for
    non-missing values is not met for all groups.

  - `"within_one"`: Features are removed if the threshold for
    non-missing values is not met for any group.

  - `"QC"`: Features are removed if the threshold for non-missing values
    is not met for the named group.

  - `"across"`: The filter is applied ignoring sample group.

  The default is `"QC"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `mv_feature_filter` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (DatasetExperiment) A DatasetExperiment object containing the filtered data. |
| `flags` | (data.frame) % missing values and a flag indicating whether the sample was rejected. 0 = rejected. |

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `mv_feature_filter` object inherits the following `struct` classes:\
\
`[mv_feature_filter]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

## Examples

``` r
M = mv_feature_filter(
      threshold = 20,
      qc_label = "QC",
      method = "QC",
      factor_name = "V1")

D = iris_DatasetExperiment()
M = mv_feature_filter(factor_name='Species',qc_label='versicolor')
M = model_apply(M,D)
```
