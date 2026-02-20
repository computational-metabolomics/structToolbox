# PLSR residuals histogram

A histogram of the residuals for a PLSR model.

## Usage

``` r
plsr_residual_hist(ycol = 1, ...)
```

## Arguments

- ycol:

  (numeric, integer, character) The y-block column to plot. The default
  is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` plsr_residual_hist ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `plsr_residual_hist` object inherits the following `struct` classes:\
\
`[plsr_residual_hist]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = plsr_residual_hist(
      ycol = 1)

C = plsr_residual_hist()
```
