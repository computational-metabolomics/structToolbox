# Dispersion ratio filter

The dispersion ratio (d-ratio) compares the standard deviation (or
non-parametric equivalent) of the Quality Control (QC) samples relative
to the standard deviation (or non-parametric equivalent) of the samples
for each feature. If the d-ratio is greater than a predefined threshold
then the observed sample variance could be due to technical variance and
the feature is removed.

## Usage

``` r
dratio_filter(
  threshold = 20,
  qc_label = "QC",
  factor_name,
  method = "ratio",
  dispersion = "sd",
  ...
)
```

## Arguments

- threshold:

  (numeric) The threshold above which features are removed. The default
  is `20`.\

- qc_label:

  (character) The label used to identify QC samples. The default is
  `"QC"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- method:

  (character) dratio method. Allowed values are limited to the
  following:

  - `"ratio"`: Dispersion of the QCs divided by the dispersion of the
    samples. Corresponds to Eq 4 in Broadhurst et al (2018).

  - `"euclidean"`: Dispersion of the QCs divided by the euclidean length
    of the total dispersion. Total dispersion is estimated from the QC
    and Sample dispersion by assuming that they are orthogonal.
    Corresponds to Eq 5 in Broadhurst et al (2018).

  The default is `"ratio"`.

- dispersion:

  (character) Dispersion method. Allowed values are limited to the
  following:

  - `"sd"`: Dispersion is estimated using the standard deviation.

  - `"mad"`: Dispersion is estimated using the median absolute
    deviation.

  The default is `"sd"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `dratio_filter` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (DatasetExperiment) A DatasetExperiment object containing the filtered data. |
| `flags` | (data.frame) Flag indicating whether the feature was rejected by the filter or not. |
| `d_ratio` | (data.frame) |

## Inheritance

A `dratio_filter` object inherits the following `struct` classes:\
\
`[dratio_filter]` \>\> `[model]` \>\> `[struct_class]`

## References

Broadhurst D, Goodacre R, Reinke SN, Kuligowski J, Wilson ID, Lewis MR,
Dunn WB (2018). "Guidelines and considerations for the use of system
suitability and quality control samples in mass spectrometry assays
applied in untargeted clinical metabolomic studies." *Metabolomics*,
*14*(6).

## Examples

``` r
M = dratio_filter(
      threshold = 20,
      qc_label = "QC",
      factor_name = "V1",
      method = "ratio",
      dispersion = "sd")

D = MTBLS79_DatasetExperiment()
M = dratio_filter(threshold=20,qc_label='QC',factor_name='Class')
M = model_apply(M,D)
```
