# Feature profile

A plot visualising the change in intensity of a feature with a
continuous variable such as time, dose, or run order.

## Usage

``` r
feature_profile_array(
  run_order,
  qc_label,
  qc_column,
  colour_by,
  feature_to_plot,
  nrow = 5,
  log = TRUE,
  ...
)
```

## Arguments

- run_order:

  (character) The sample-meta column name containing run order.

- qc_label:

  (character) The label used to identify QC samples.

- qc_column:

  (character) The sample-meta column name containing the labels used to
  identify QC samples.

- colour_by:

  (character) The sample-meta column name to used to colour the plot.

- feature_to_plot:

  (numeric, character, integer) The name or column id of the plotted
  feature.

- nrow:

  (numeric, integer) The number of rows in the plot. The default is
  `5`.\

- log:

  (logical) Log transform. Allowed values are limited to the following:

  - `"TRUE"`: The data is log tranformed before plotting.

  - `"FALSE"`: The data is not transformed before plotting.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` feature_profile_array ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Inheritance

A `feature_profile_array` object inherits the following `struct`
classes:\
\
`[feature_profile_array]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = feature_profile_array(
      run_order = character(0),
      qc_label = character(0),
      qc_column = character(0),
      colour_by = character(0),
      feature_to_plot = numeric(0),
      nrow = 1,
      log = FALSE)

D = MTBLS79_DatasetExperiment()
C = feature_profile_array(
    run_order='run_order',
    qc_label='QC',
    qc_column='Class',
    colour_by='Class',
    feature_to_plot=1:3,
    nrow=1,
    log=TRUE)
chart_plot(C,D)
#> Warning: Removed 23 rows containing missing values or values outside the scale range
#> (`geom_point()`).
```
