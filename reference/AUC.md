# Area under ROC curve

The area under the ROC curve of a classifier is estimated using the
trapezoid method.

## Usage

``` r
AUC(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` AUC ` object. This object has no `output` slots.

## Inheritance

A `AUC` object inherits the following `struct` classes:\
\
`[AUC]` \>\> `[metric]` \>\> `[struct_class]`

## Examples

``` r
M = AUC()

D = iris_DatasetExperiment()
XCV = kfold_xval(folds=5,factor_name='Species') *
      (mean_centre() + PLSDA(number_components=2,factor_name='Species'))
MET = AUC()
XCV = run(XCV,D,MET)
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
#> Warning: ‘>=’ not meaningful for factors
```
