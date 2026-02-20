# Missing value sample filter

Removes samples where the percent number of missing values exceeds a
threshold.

## Usage

``` r
mv_sample_filter(mv_threshold = 20, ...)
```

## Arguments

- mv_threshold:

  (numeric) The maximum percentage of features with missing values in a
  sample. The default is `20`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `mv_sample_filter` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (DatasetExperiment) A DatasetExperiment object containing the filtered data. |
| `flags` | (data.frame) A flag indicating whether the sample was rejected. 0 = rejected. |
| `percent_missing` | (data.frame) % missing values for each sample. |

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `mv_sample_filter` object inherits the following `struct` classes:\
\
`[mv_sample_filter]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

## Examples

``` r
M = mv_sample_filter(
      mv_threshold = 20)

C = mv_sample_filter()
```
