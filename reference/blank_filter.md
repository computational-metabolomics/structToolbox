# Blank filter

A blank filter filters features by comparing the median intensity of
blank samples to the median intensity of samples. Features where the
relative intensity (fold change) is not large when compared to the blank
are removed. The number of times a feature is detected across all blank
samples may also be considered. If the feature is not detected in a high
enough proportion of the blanks then it is not removed.

## Usage

``` r
blank_filter(
  fold_change = 20,
  blank_label = "blank",
  qc_label = "QC",
  factor_name,
  fraction_in_blank = 0,
  ...
)
```

## Arguments

- fold_change:

  (numeric) Features with fold change less than this value are removed.
  The default is `20`.\

- blank_label:

  (character) The label used to identify blank samples. The default is
  `"blank"`.

- qc_label:

  (character, NULL) The label used to identify QC samples. If set to
  NULL then the median of the samples is used. The default is `"QC"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- fraction_in_blank:

  (numeric) Features present in less than this proportion of the blanks
  are not considered for removal. The default is `0`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `blank_filter` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (DatasetExperiment) A DatasetExperiment object containing the filtered data. |
| `flags` | (data.frame) A flag indicating whether the feature was rejected or not. |

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `blank_filter` object inherits the following `struct` classes:\
\
`[blank_filter]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

## Examples

``` r
M = blank_filter(
      fold_change = 20,
      blank_label = "Blank",
      qc_label = "QC",
      factor_name = "V1",
      fraction_in_blank = 0)

D = iris_DatasetExperiment()
M = blank_filter(fold_change=2,
                 factor_name='Species',
                 blank_label='setosa',
                 qc_label='versicolor')
M = model_apply(M,D)
```
