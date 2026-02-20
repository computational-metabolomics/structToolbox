# d-statistic plot

A bar chart of the d-statistics for samples in the input PCA model.
Samples above the indicated threshold are considered to be outlying.

## Usage

``` r
pca_dstat_plot(number_components = 2, alpha = 0.05, ...)
```

## Arguments

- number_components:

  (numeric) The number of principal components to use. The default is
  `2`.\

- alpha:

  (numeric) A confidence threshold for rejecting samples based on the
  d-statistic. The default is `0.05`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pca_dstat_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `pca_dstat_plot` object inherits the following `struct` classes:\
\
`[pca_dstat_plot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = pca_dstat_plot(
      number_components = 2,
      alpha = 0.95)

C = pca_dstat_plot()
```
