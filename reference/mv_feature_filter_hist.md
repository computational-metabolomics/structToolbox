# Histogram of missing values per feature

A histogram of the proportion of missing values per feature.

## Usage

``` r
mv_feature_filter_hist(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` mv_feature_filter_hist ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Inheritance

A `mv_feature_filter_hist` object inherits the following `struct`
classes:\
\
`[mv_feature_filter_hist]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = mv_feature_filter_hist()

C = mv_feature_filter_hist()
```
