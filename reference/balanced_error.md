# Balanced error

Balanced Accuracy is the average proportion of correctly identified
samples within each class. Balanced error is 1 - Balanced Accuracy.

## Usage

``` r
balanced_error(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` balanced_error ` object. This object has no `output` slots.

## Inheritance

A `balanced_error` object inherits the following `struct` classes:\
\
`[balanced_error]` \>\> `[metric]` \>\> `[struct_class]`

## Examples

``` r
M = balanced_error()

D = iris_DatasetExperiment()
XCV = kfold_xval(folds=5,factor_name='Species') *
      (mean_centre() + PLSDA(number_components=2,factor_name='Species'))
MET = balanced_error()
XCV = run(XCV,D,MET)
```
