# Equal group sized sampling

Samples are randomly chosen from each level such that the training set
has equal numbers of samples for all levels. The number of samples is
based on the input proportion and the smallest group size.

## Usage

``` r
equal_split(p_train = 1, factor_name, ...)
```

## Arguments

- p_train:

  (numeric) The proportion of samples selected for the training set. The
  default is `1`.\

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `equal_split` object with the following `output` slots:

|  |  |
|----|----|
| `training` | (DatasetExperiment) A DatasetExperiment object containing samples selected for the training set. |
| `testing` | (DatasetExperiment) A DatasetExperiment object containing samples selected for the testing set. |

## Inheritance

A `equal_split` object inherits the following `struct` classes:\
\
`[equal_split]` \>\> `[split_data]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = equal_split(
      factor_name = "V1",
      p_train = 0.75)

D = iris_DatasetExperiment()
M = equal_split(factor_name='Species')
M = model_apply(M,D)
```
