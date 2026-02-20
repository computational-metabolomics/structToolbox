# kfoldxcv metric plot

A boxplot of the performance metric computed for each fold of a k-fold
cross-validation.

## Usage

``` r
kfoldxcv_metric(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` kfoldxcv_metric ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `kfoldxcv_metric` object inherits the following `struct` classes:\
\
`[kfoldxcv_metric]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = kfoldxcv_metric()

C = kfoldxcv_metric()
```
