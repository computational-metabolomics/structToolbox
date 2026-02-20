# PLSR QQ plot

A plot of the quantiles of the residuals from a PLSR model against the
quantiles of a normal distribution.

## Usage

``` r
plsr_qq_plot(ycol = 1, ...)
```

## Arguments

- ycol:

  (numeric, integer, character) The y-block column to plot. The default
  is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` plsr_qq_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `plsr_qq_plot` object inherits the following `struct` classes:\
\
`[plsr_qq_plot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = plsr_qq_plot(
      ycol = 1)

C = plsr_qq_plot()
```
