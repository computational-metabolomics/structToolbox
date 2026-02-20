# Grid search line plot

A plot of the calculated performance metric against the model input
parameter values used to train the model. The optimum parameter value is
indicated based on minimising (or maximising) the chosen metric.

## Usage

``` r
gs_line(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` gs_line ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `gs_line` object inherits the following `struct` classes:\
\
`[gs_line]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = gs_line()

C = gs_line()
```
