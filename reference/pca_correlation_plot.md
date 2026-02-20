# PCA correlation plot

A plot of the correlation between the variables/features and the
selected principal component scores. Features with high correlation are
well represented by the selected component(s)

## Usage

``` r
pca_correlation_plot(components = c(1, 2), ...)
```

## Arguments

- components:

  (numeric) The Principal Components used to generate the plot. The
  default is `c(1, 2)`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pca_correlation_plot ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Inheritance

A `pca_correlation_plot` object inherits the following `struct`
classes:\
\
`[pca_correlation_plot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = pca_correlation_plot(
      components = c(1, 2))

C = pca_correlation_plot()
```
