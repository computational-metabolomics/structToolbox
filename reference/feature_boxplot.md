# Feature boxplot

A boxplot to visualise the distribution of values within a feature.

## Usage

``` r
feature_boxplot(
  label_outliers = TRUE,
  feature_to_plot,
  factor_name,
  show_counts = TRUE,
  style = "boxplot",
  jitter = FALSE,
  fill = FALSE,
  ...
)
```

## Arguments

- label_outliers:

  (logical) Label outliers. Allowed values are limited to the following:

  - `"TRUE"`: The index for outlier samples is included on the plot.

  - `"FALSE"`: No labels are displayed.

  The default is `TRUE`.\

- feature_to_plot:

  (character, numeric, integer) The column name of the plotted feature.

- factor_name:

  (character) The name of a sample-meta column to use.

- show_counts:

  (logical) Show counts. Allowed values are limited to the following:

  - `"TRUE"`: The number of samples for each box is displayed.

  - `"FALSE"`: The number of samples for each box is not displayed.

  The default is `TRUE`.\

- style:

  (character) Plot style. Allowed values are limited to the following:

  - `"boxplot"`: Boxplot style.

  - `"violin"`: Violon plot style.

  The default is `"boxplot"`.

- jitter:

  (logical) Include points plotted with added jitter. The default is
  `FALSE`.\

- fill:

  (logical) Block fill the boxes or violins with the group colour. The
  default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` feature_boxplot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `feature_boxplot` object inherits the following `struct` classes:\
\
`[feature_boxplot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = feature_boxplot(
      label_outliers = FALSE,
      feature_to_plot = "V1",
      factor_name = "V1",
      show_counts = FALSE,
      style = "boxplot",
      jitter = FALSE,
      fill = FALSE)

D = MTBLS79_DatasetExperiment
C = feature_boxplot(factor_name='Species',feature_to_plot='Petal.Width')
chart_plot(C,D)
#> Warning: no chart defined for "function"
#> A "feature_boxplot" object
#> --------------------------
#> name:          Feature boxplot
#> description:   A boxplot to visualise the distribution of values within a feature.
#> input params:  label_outliers, feature_to_plot, factor_name, show_counts, style, jitter, fill 
```
