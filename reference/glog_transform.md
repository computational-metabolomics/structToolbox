# Generalised logarithmic transform

The generalised logarithm (glog) transformation applies a log
transformation while applying an offset to account for technical
variation.

## Usage

``` r
glog_transform(qc_label = "QC", factor_name, lambda = NULL, ...)
```

## Arguments

- qc_label:

  (character) The label used to identify QC samples. The default is
  `"QC"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- lambda:

  (numeric, NULL) The value of lambda to use. If NULL then the pmp
  package will be used to determine an "optimal" value for lambda. The
  default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `glog_transform` object with the following `output` slots:

|  |  |
|----|----|
| `transformed` | (DatasetExperiment) A DatasetExperiment object containing the glog transformed data. |
| `error_flag` | (logical) A logical indicating whether the glog optimisation for lambda was successful. If not then PMP returns a default value for lambda. |

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `glog_transform` object inherits the following `struct` classes:\
\
`[glog_transform]` \>\> `[model]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

Durbin B, Hardin J, Hawkins D, Rocke D (2002). "A variance-stabilizing
transformation for gene-expression microarray data." *Bioinformatics*,
*18*(Suppl 1), S105-S110.

Parsons HM, Ludwig C, Gunther UL, Viant MR (2007). "Improved
classification accuracy in 1- and ', '2-dimensional NMR metabolomics
data using the variance ', 'stabilising generalised logarithm
transformation." *Bioinformatics*, *8*(1), 234.

## Examples

``` r
M = glog_transform(
      qc_label = "QC",
      factor_name = "V1",
      lambda = NULL)

D = iris_DatasetExperiment()
M = glog_transform(qc_label='versicolor',factor_name='Species')
M = model_apply(M,D)
#> Error!Lambda tending to infinity! Using standard
#> Error!Lambda tending to infinity! Using standard
```
