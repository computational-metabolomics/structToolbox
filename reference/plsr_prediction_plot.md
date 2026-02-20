# PLSR prediction plot

A scatter plot of the true response values against the predicted values
for a PLSR model.

## Usage

``` r
plsr_prediction_plot(ycol = 1, ...)
```

## Arguments

- ycol:

  (numeric, integer, character) The y-block column to plot. The default
  is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` plsr_prediction_plot ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Inheritance

A `plsr_prediction_plot` object inherits the following `struct`
classes:\
\
`[plsr_prediction_plot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = plsr_prediction_plot(
      ycol = 1)

C = plsr_prediction_plot()
```
