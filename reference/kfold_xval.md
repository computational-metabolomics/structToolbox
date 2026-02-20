# k-fold cross-validation

k-fold cross-validation is an iterative approach applied to validate
models. The samples are divided into k "folds", or subsets. Each subset
is excluded from model training and used for model validation once,
resulting in a single left-out prediction for each sample. Model
performance metrics are then computed for the training and test sets
across all folds.

## Usage

``` r
kfold_xval(folds = 10, method = "venetian", factor_name, collect = NULL, ...)
```

## Arguments

- folds:

  (numeric, integer) The number of cross-validation folds. The default
  is `10`.\

- method:

  (character) Fold selection method. Allowed values are limited to the
  following:

  - `"venetian"`: Every nth sample is assigned to the same fold, where n
    is the number of folds.

  - `"blocks"`: Blocks of adjacent samples are assigned to the same
    fold.

  - `"random"`: Samples are randomly assigned to a fold.

  The default is `"venetian"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- collect:

  (NULL, character) The name of a model output to collect over all
  bootstrap repetitions, in addition to the input metric. The default is
  `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `kfold_xval` object with the following `output` slots:

|                |              |
|----------------|--------------|
| `results`      | (data.frame) |
| `metric`       | (data.frame) |
| `metric.train` | (numeric)    |
| `metric.test`  | (numeric)    |
| `collected`    | (list)       |

## Inheritance

A `kfold_xval` object inherits the following `struct` classes:\
\
`[kfold_xval]` \>\> `[resampler]` \>\> `[iterator]` \>\>
`[struct_class]`

## Examples

``` r
M = kfold_xval(
      folds = 5,
      method = "random",
      factor_name = "V1",
      collect = NULL)

D = iris_DatasetExperiment()
I = kfold_xval(factor_name='Species') *
    (mean_centre() + PLSDA(factor_name='Species'))
I = run(I,D,balanced_accuracy())
```
