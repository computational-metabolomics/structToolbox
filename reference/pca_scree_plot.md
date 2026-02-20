# Scree plot

A plot of the percent variance and cumulative percent variance for the
components of a PCA model.

## Usage

``` r
pca_scree_plot(max_pc = 15, ...)
```

## Arguments

- max_pc:

  (numeric, integer) The maximum number of components to include in the
  plot. The default is `15`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pca_scree_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

struct object

## Inheritance

A `pca_scree_plot` object inherits the following `struct` classes:\
\
`[pca_scree_plot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = pca_scree_plot(
      max_pc = 15)

C = pca_scree_plot()
```
