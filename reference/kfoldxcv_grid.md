# k-fold cross validation plot

A graphic for visualising the true class and the predicted class of
samples in all groups for all cross-validation folds.

## Usage

``` r
kfoldxcv_grid(factor_name, level, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- level:

  (character) The level/group to plot.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` kfoldxcv_grid ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `kfoldxcv_grid` object inherits the following `struct` classes:\
\
`[kfoldxcv_grid]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = kfoldxcv_grid(
      factor_name = "V1",
      level = "level_1")

D = iris_DatasetExperiment()
I = kfold_xval(factor_name='Species') *
    (mean_centre() + PLSDA(factor_name='Species'))
I = run(I,D,balanced_accuracy())

C = kfoldxcv_grid(factor_name='Species',level='setosa')
chart_plot(C,I)

```
