# DatasetExperiment heatmap

A heatmap to visualise the measured values in a data matrix.

## Usage

``` r
DatasetExperiment_heatmap(na_colour = "#FF00E4", ...)
```

## Arguments

- na_colour:

  (character) The hex colour code used to plot missing values. The
  default is `"#FF00E4"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` DatasetExperiment_heatmap ` object. This object has no `output`
slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `reshape2`

## Inheritance

A `DatasetExperiment_heatmap` object inherits the following `struct`
classes:\
\
`[DatasetExperiment_heatmap]` \>\> `[chart]` \>\> `[struct_class]`

## References

Wickham H (2007). "Reshaping Data with the reshape Package." *Journal of
Statistical Software*, *21*(12), 1-20.
<https://www.jstatsoft.org/v21/i12/>.

## Examples

``` r
M = DatasetExperiment_heatmap(
      na_colour = "#FF00E4")

D = iris_DatasetExperiment()
C = DatasetExperiment_heatmap()
chart_plot(C,D)
```
