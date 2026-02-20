# Mean of medians

The data matrix is normalised by the mean of the median of each factor
level.

## Usage

``` r
mean_of_medians(factor_name, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `mean_of_medians` object with the following `output` slots:

|  |  |
|----|----|
| `transformed` | (DatasetExperiment) Data after the tranformation has been applied. |

## Inheritance

A `mean_of_medians` object inherits the following `struct` classes:\
\
`[mean_of_medians]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = mean_of_medians(
      factor_name = "V1")

D = iris_DatasetExperiment()
M = mean_of_medians(factor_name='Species')
M = model_apply(M,D)
```
