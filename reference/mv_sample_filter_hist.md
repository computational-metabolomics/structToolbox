# Histogram of missing values per sample

A histogram of the the proportion of missing values per sample

## Usage

``` r
mv_sample_filter_hist(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` mv_sample_filter_hist ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Inheritance

A `mv_sample_filter_hist` object inherits the following `struct`
classes:\
\
`[mv_sample_filter_hist]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = mv_sample_filter_hist()

C = mv_sample_filter_hist()
```
