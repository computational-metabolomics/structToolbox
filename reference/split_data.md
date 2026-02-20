# Split data

The data matrix is divided into two subsets.A predefined proportion of
the samples are randomly selected for a training set, and the remaining
samples are used for the test set.

## Usage

``` r
split_data(p_train, ...)
```

## Arguments

- p_train:

  (numeric) The proportion of samples selected for the training set.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `split_data` object with the following `output` slots:

|  |  |
|----|----|
| `training` | (DatasetExperiment) A DatasetExperiment object containing samples selected for the training set. |
| `testing` | (DatasetExperiment) A DatasetExperiment object containing samples selected for the testing set. |

## Inheritance

A `split_data` object inherits the following `struct` classes:\
\
`[split_data]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = split_data(
      p_train = 0.75)

M = split_data(p_train=0.75)
```
