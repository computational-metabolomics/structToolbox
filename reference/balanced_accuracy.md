# Balanced Accuracy

Balanced Accuracy is the average proportion of correctly identified
samples within each class.

## Usage

``` r
balanced_accuracy(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` balanced_accuracy ` object. This object has no `output` slots.

## Inheritance

A `balanced_accuracy` object inherits the following `struct` classes:\
\
`[balanced_accuracy]` \>\> `[metric]` \>\> `[struct_class]`

## Examples

``` r
M = balanced_accuracy()

D = iris_DatasetExperiment()
XCV = kfold_xval(folds=5,factor_name='Species') *
      (mean_centre() + PLSDA(number_components=2,factor_name='Species'))
MET = balanced_accuracy()
XCV = run(XCV,D,MET)
```
