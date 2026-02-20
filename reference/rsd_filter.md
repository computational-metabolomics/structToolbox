# RSD filter

An RSD filter calculates the relative standard deviation (the ratio of
the standard deviation to the mean) for all features. Any feature with
an RSD greater than a predefined threshold is excluded.

## Usage

``` r
rsd_filter(rsd_threshold = 20, qc_label = "QC", factor_name, ...)
```

## Arguments

- rsd_threshold:

  (numeric) The RSD threshold above which features are removed. The
  default is `20`.\

- qc_label:

  (character) The label used to identify QC samples. The default is
  `"QC"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `rsd_filter` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (DatasetExperiment) A DatasetExperiment object containing the filtered data. |
| `flags` | (data.frame) RSD and a flag indicating whether the feature was rejected by the filter or not. |
| `rsd_qc` | (data.frame) The calculated RSD of the QC class. |

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `rsd_filter` object inherits the following `struct` classes:\
\
`[rsd_filter]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

## Examples

``` r
M = rsd_filter(
      rsd_threshold = 20,
      qc_label = "QC",
      factor_name = "V1")

M = rsd_filter(factor_name='Class')
```
