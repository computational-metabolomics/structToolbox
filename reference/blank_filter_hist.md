# Histogram of blank filter fold changes

A histogram of the calculated fold changes for the blank filter (median
samples divided by median blanks)

## Usage

``` r
blank_filter_hist(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` blank_filter_hist ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `blank_filter_hist` object inherits the following `struct` classes:\
\
`[blank_filter_hist]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = blank_filter_hist()

C = blank_filter_hist()
```
