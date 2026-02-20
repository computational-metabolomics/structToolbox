# Fold change for interactions between factors

For more than one factor the fold change calculation is extended to
include all combinations of levels (interactions) of all factors. Paired
fold changes are not possible for this computation.

## Usage

``` r
fold_change_int(
  factor_name,
  threshold = 2,
  control_group = character(0),
  method = "geometric",
  conf_level = 0.95,
  ...
)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- threshold:

  (numeric) The fold change threshold for labelling features as
  significant. The default is `2`.\

- control_group:

  (character) The level names of the groups used in the denominator
  (where possible) when computing fold change. One level for each
  factor, assumed to be in the same order as factor_name. The default is
  `character(0)`.

- method:

  (character) Fold change method. Allowed values are limited to the
  following:

  - `"geometric"`: A log transform is applied before using group means
    to calculate fold change. In the non-tranformedspace this is
    equivalent to using geometric group means. Confidence intervals for
    independant and paired sampling are estimated using standard error
    of the mean in log transformed space before being transformed back
    to the original space.

  - `"median"`: The group medians and the method described by Price and
    Bonett is used to estimate confidence intervals. For paired data
    standard error of the median is used to estimate confidence
    intervals from the median fold change of all pairs.

  - `"mean"`: The group means and the method described by Price and
    Bonnet is used to estimate confidence intervals. For paired data
    standard error of the mean is used to estimate confidence intervals
    from the mean fold change of all pairs.

  The default is `"geometric"`.

- conf_level:

  (numeric) The confidence level of the interval. The default is
  `0.95`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `fold_change_int` object with the following `output` slots:

|  |  |
|----|----|
| `fold_change` | (data.frame) The fold change between groups. |
| `lower_ci` | (data.frame) Lower confidence interval for fold change. |
| `upper_ci` | (data.frame) Upper confidence interval for fold change. |
| `significant` | (data.frame) A logical indictor of whether the calculated fold change including the estimated confidence limits is greater than the selected threshold. |

## Inheritance

A `fold_change_int` object inherits the following `struct` classes:\
\
`[fold_change_int]` \>\> `[fold_change]` \>\> `[model]` \>\>
`[struct_class]`

## References

Lloyd GR, Jankevics A, Weber RJM (2020). "struct: an
R/Bioconductor-based framework for standardized metabolomics data
analysis and beyond." *Bioinformatics*, *36*(22-23), 5551-5552.
<https://doi.org/10.1093/bioinformatics/btaa1031>.

## Examples

``` r
M = fold_change_int(
      factor_name = "V1",
      sample_name = character(0),
      threshold = 2,
      control_group = character(0),
      method = "geometric",
      paired = FALSE,
      conf_level = 0.95)

D = MTBLS79_DatasetExperiment()
D=D[,1:10,drop=FALSE]
M = filter_smeta(mode='exclude',levels='QC',factor_name='Class') +
    fold_change_int(factor_name=c('Class','Batch'))
M = model_apply(M,D)
```
