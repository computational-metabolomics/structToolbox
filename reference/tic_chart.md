# Total Ion Count chart.

A scatter plot of Total Ion Count (sum of each sample) versus run order.

## Usage

``` r
tic_chart(run_order, factor_name, connected = FALSE, ...)
```

## Arguments

- run_order:

  (character) The column name of sample_meta indicating the run order of
  the samples.

- factor_name:

  (character) The name of a sample-meta column to use.

- connected:

  (logical) Plot samples connected by a grey line. The default is
  `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` tic_chart ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `tic_chart` object inherits the following `struct` classes:\
\
`[tic_chart]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = tic_chart(
      factor_name = "V1",
      run_order = character(0),
      connected = FALSE)

D = iris_DatasetExperiment()
D$sample_meta$run_order=1:nrow(D)
C = tic_chart(factor_name='Species',run_order='run_order')
chart_plot(C,D)

```
