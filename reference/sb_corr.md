# Signal/batch correction for mass spectrometry data

Applies Quality Control Robust Spline (QC-RSC) method to correct for
signal drift and batch differences in mass spectrometry data.

## Usage

``` r
sb_corr(
  order_col,
  batch_col,
  qc_col,
  smooth = 0,
  use_log = TRUE,
  min_qc = 4,
  qc_label = "QC",
  spar_lim = c(-1.5, 1.5),
  ...
)
```

## Arguments

- order_col:

  (character) The column name of sample_meta indicating the run order of
  the samples.

- batch_col:

  (character) The column name of sample_meta indicating the batch each
  sample was measured in.

- qc_col:

  (character) The column name of sample_meta indicating the group each
  sample is a member of.

- smooth:

  (numeric) The amount of smoothing applied (0 to 1). If set to 0 the
  smoothing parameter will be estimated using leave-one-out
  cross-validation. The default is `0`.\

- use_log:

  (logical) Log tranformation. Allowed values are limited to the
  following:

  - `"TRUE"`: The data is log transformed prior to performing signal
    correction.

  - `"FALSE"`: Signal correction is applied to the input data.

  The default is `TRUE`.\

- min_qc:

  (numeric) The minimum number of QC samples required for signal
  correction. The default is `4`.\

- qc_label:

  (character) The label used to identify QC samples. The default is
  `"QC"`.

- spar_lim:

  (numeric) A two element vector specifying the upper and lower limits
  when `spar = 0`. Allows the value of `spar` to be constrained within
  these limits to prevent overfitting. The default is `c(-1.5, 1.5)`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `sb_corr` object with the following `output` slots:

|  |  |
|----|----|
| `corrected` | (DatasetExperiment) The DatasetExperiment after signal/batch correction has been applied. |
| `fitted` | (data.frame) The fitted splines for each feature. |

struct object

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `sb_corr` object inherits the following `struct` classes:\
\
`[sb_corr]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

Kirwan JA, Broadhurst DI, Davidson RL, Viant MR (2013). "Characterising
and correcting batch variation in an automated direct infusion mass
spectrometry (DIMS) metabolomics workflow." *Analytical and
Bioanalytical Chemistry*, *405*(15), 5147-5157.

## Examples

``` r
M = sb_corr(
      order_col = character(0),
      batch_col = character(0),
      qc_col = character(0),
      smooth = 0,
      use_log = FALSE,
      min_qc = 4,
      qc_label = "QC",
      spar_lim = c(-1.5, 1.5))

M = sb_corr(order_col='run_order',batch_col='batch_no',qc_col='class')
```
