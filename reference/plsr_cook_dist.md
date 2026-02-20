# Cook's distance barchart

A barchart of Cook's distance for each sample used to train a PLSR
model. Cook's distance is used to estimate the influence of a sample on
the model and can be used to identify potential outliers.

## Usage

``` r
plsr_cook_dist(ycol = 1, ...)
```

## Arguments

- ycol:

  (numeric, integer, character) The y-block column to plot. The default
  is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` plsr_cook_dist ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `plsr_cook_dist` object inherits the following `struct` classes:\
\
`[plsr_cook_dist]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = plsr_cook_dist(
      ycol = 1)

C = plsr_cook_dist()
```
