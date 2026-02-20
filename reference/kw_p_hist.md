# Histogram of p values

A histogram of the p-values computed by the kruskal-wallis method

## Usage

``` r
kw_p_hist(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` kw_p_hist ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `kw_p_hist` object inherits the following `struct` classes:\
\
`[kw_p_hist]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = kw_p_hist()

C = kw_p_hist()
```
