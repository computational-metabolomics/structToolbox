# Probabilistic Quotient Normalisation (PQN)

PQN is used to normalise for differences in concentration between
samples. It makes use of Quality Control (QC) samples as a reference.
PQN scales by the median change relative to the reference in order to be
more robust against changes caused by response to perturbation.

## Usage

``` r
pqn_norm(
  qc_label = "QC",
  factor_name,
  qc_frac = 0,
  sample_frac = 0,
  ref_method = "mean",
  ref_mean = NULL,
  ...
)
```

## Arguments

- qc_label:

  (character) The label used to identify QC samples. The default is
  `"QC"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- qc_frac:

  (numeric) A value between 0 and 1 to indicate the minimum proportion
  of QC samples a feature must be present in for it to be included when
  computing the reference. Default qc_frac = 0. . The default is `0`.\

- sample_frac:

  (numeric) A value between 0 and 1 to indicate the minimum proportion
  of samples a feature must be present in for it to be considered when
  computing the normalisation coefficients. . The default is `0`.\

- ref_method:

  (character) Reference computation method. Allowed values are limited
  to the following:

  - `"mean"`: The reference is computed as the mean of the samples
    matching the qc_label input.

  - `"median"`: The reference is computed as the median of the samples
    matching the qc_label_input.

  The default is `"mean"`.

- ref_mean:

  (numeric, NULL) A single sample to use as the reference for
  normalisation. If set to NULL then the reference will be computed
  based on the other input parameters (ref_mean, qc_label etc). . The
  default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `pqn_norm` object with the following `output` slots:

|  |  |
|----|----|
| `normalised` | (DatasetExperiment) A DatasetExperiment object containing the normalised data. |
| `coeff` | (data.frame) The normalisation coefficients calculated by PQN. |

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `pqn_norm` object inherits the following `struct` classes:\
\
`[pqn_norm]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

## Examples

``` r
M = pqn_norm(
      qc_label = "QC",
      factor_name = "V1",
      qc_frac = 0,
      sample_frac = 0,
      ref_mean = NULL,
      ref_method = "mean")

D = iris_DatasetExperiment()
M = pqn_norm(factor_name='Species',qc_label='all')
M = model_apply(M,D)
```
