# Stratified sampling

The dataset is divided into two subsets. A predefined proportion of
samples from each level of a factor is selected for the training set,
and the remaining samples are used for the test set. The stratification
by factor level means that the relative number of samples per level is
approximately equal to the original dataset.

## Usage

``` r
stratified_split(p_train, factor_name, ...)
```

## Arguments

- p_train:

  (numeric) The proportion of samples selected for the training set.

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `stratified_split` object with the following `output` slots:

|  |  |
|----|----|
| `training` | (DatasetExperiment) A DatasetExperiment object containing samples selected for the training set. |
| `testing` | (DatasetExperiment) A DatasetExperiment object containing samples selected for the testing set. |

## Inheritance

A `stratified_split` object inherits the following `struct` classes:\
\
`[stratified_split]` \>\> `[split_data]` \>\> `[model]` \>\>
`[struct_class]`

## Examples

``` r
M = stratified_split(
      factor_name = "V1",
      p_train = 0.75)

D = iris_DatasetExperiment()
M = stratified_split(p_train=0.75,factor_name='Species')
M = model_apply(M,D)
```
