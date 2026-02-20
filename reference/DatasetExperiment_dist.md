# Feature distribution histogram

A histogram to visualise the distribution of values within features.

## Usage

``` r
DatasetExperiment_dist(factor_name, per_class = TRUE, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- per_class:

  (logical) Plot per class. Allowed values are limited to the following:

  - `"TRUE"`: The distributions are plotted for each class.

  - `"FALSE"`: The distribution is plotted for all samples.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` DatasetExperiment_dist ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Inheritance

A `DatasetExperiment_dist` object inherits the following `struct`
classes:\
\
`[DatasetExperiment_dist]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = DatasetExperiment_dist(
      factor_name = "V1",
      per_class = FALSE)

D = MTBLS79_DatasetExperiment()
C = DatasetExperiment_dist(factor_name='Class')
chart_plot(C,D)
#> Warning: Removed 9222 rows containing non-finite outside the scale range (`stat_bin()`).
```
