# Factor boxplot

Boxplot for a feature to visualise the distribution of values within
each group

## Usage

``` r
DatasetExperiment_factor_boxplot(feature_to_plot, factor_names, ...)
```

## Arguments

- feature_to_plot:

  (character, numeric, integer) The name of the plotted feature.

- factor_names:

  (character) The name of sample meta column(s) to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` DatasetExperiment_factor_boxplot ` object. This object has no
`output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `DatasetExperiment_factor_boxplot` object inherits the following
`struct` classes:\
\
`[DatasetExperiment_factor_boxplot]` \>\> `[chart]` \>\>
`[struct_class]`

## Examples

``` r
M = DatasetExperiment_factor_boxplot(
      factor_names = "V1",
      feature_to_plot = "V1")

D = iris_DatasetExperiment()
C = DatasetExperiment_factor_boxplot(factor_names='Species',feature_to_plot='Petal.Width')
chart_plot(C,D)
```
