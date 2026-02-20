# PLSDA scores plot

A scatter plot of the selected PLSDA scores.

## Usage

``` r
pls_scores_plot(
  xcol = "LV1",
  ycol = "LV2",
  points_to_label = "none",
  factor_name,
  ellipse = "all",
  ellipse_type = "norm",
  ellipse_confidence = 0.95,
  label_filter = character(0),
  label_factor = "rownames",
  label_size = 3.88,
  components = NULL,
  ...
)

plsda_scores_plot(
  xcol = "LV1",
  ycol = "LV2",
  points_to_label = "none",
  factor_name,
  ellipse = "all",
  ellipse_type = "norm",
  ellipse_confidence = 0.95,
  label_filter = character(0),
  label_factor = "rownames",
  label_size = 3.88,
  components = NULL,
  ...
)
```

## Arguments

- xcol:

  (numeric, integer, character) The column name, or index, of data to
  plot on the x-axis. The default is `"LV1"`.

- ycol:

  (numeric, integer, character) The column name, or index, of data to
  plot on the y-axis. The default is `"LV2"`.

- points_to_label:

  (character) Points to label. Allowed values are limited to the
  following:

  - `"none"`: No samples labels are displayed.

  - `"all"`: The labels for all samples are displayed.

  - `"outliers"`: Labels for for potential outlier samples are
    displayed.

  The default is `"none"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- ellipse:

  (character) Plot ellipses. Allowed values are limited to the
  following:

  - `"all"`: Ellipses are plotted for all groups and all samples.

  - `"group"`: Ellipses are plotted for all groups.

  - `"none"`: Ellipses are not included on the plot.

  - `"sample"`: An ellipse is plotted for all samples (ignoring group).

  The default is `"all"`.

- ellipse_type:

  (character) Type of ellipse. Allowed values are limited to the
  following:

  - `"norm"`: Multivariate normal (p = 0.95).

  - `"t"`: Multivariate t (p = 0.95).

  The default is `"norm"`.

- ellipse_confidence:

  (numeric) The confidence level for plotting ellipses. The default is
  `0.95`.\

- label_filter:

  (character) Labels are only plotted for the named groups. If
  zero-length then all groups are included. The default is
  `character(0)`.

- label_factor:

  (character) The column name of sample_meta to use for labelling
  samples on the plot. "rownames" will use the row names from
  sample_meta. The default is `"rownames"`.

- label_size:

  (numeric) The text size of labels. Note this is not in Font Units. The
  default is `3.88`.\

- components:

  (numeric, integer, NULL) The principal components used to generate the
  plot. If provided this parameter overrides xcol and ycol params. The
  default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pls_scores_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `pls_scores_plot` object inherits the following `struct` classes:\
\
`[pls_scores_plot]` \>\> `[scatter_chart]` \>\> `[chart]` \>\>
`[struct_class]`

## Examples

``` r
M = pls_scores_plot(
      components = NULL,
      xcol = 1,
      ycol = 2,
      points_to_label = "none",
      factor_name = "V1",
      ellipse = "all",
      label_filter = character(0),
      label_factor = "rownames",
      label_size = 3.88,
      ellipse_type = "norm",
      ellipse_confidence = 0.95)

D = iris_DatasetExperiment()
M = mean_centre()+PLSDA(factor_name='Species')
M = model_apply(M,D)

C = pls_scores_plot(factor_name='Species')
chart_plot(C,M[2])
```
