# Fold change plot

A plot of fold changes calculated for a chosen subset of features. A
predefined fold change threshold is indicated by shaded regions.

## Usage

``` r
fold_change_plot(number_features = 20, orientation = "portrait", ...)
```

## Arguments

- number_features:

  (numeric) The number randomly selected features to plot, or a list of
  column numbers. The default is `20`.\

- orientation:

  (character) Plot orientation. Allowed values are limited to the
  following:

  - `"landscape"`: Features are plotted on the y-axis.

  - `"portrait"`: Features are plotted on the x-axis.

  The default is `"portrait"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` fold_change_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `fold_change_plot` object inherits the following `struct` classes:\
\
`[fold_change_plot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = fold_change_plot(
      number_features = 10,
      orientation = "portrait")

C = fold_change_plot()
```
