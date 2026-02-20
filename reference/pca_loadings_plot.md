# PCA loadings plot

A barchart (one component) or scatter plot (two components) of the
selected principal component loadings.

## Usage

``` r
pca_loadings_plot(
  components = c(1, 2),
  style = "points",
  label_features = NULL,
  ...
)
```

## Arguments

- components:

  (numeric) The principal components used to generate the plot. The
  default is `c(1, 2)`.

- style:

  (character) Plot style. Allowed values are limited to the following:

  - `"points"`: Loadings and scores are plotted as a scatter plot.

  - `"arrows"`: The loadings are plotted as arrow vectors.

  The default is `"points"`.

- label_features:

  (character, NULL) Feature labels. Allowed values are limited to the
  following:

  - `"character()"`: A vector of labels for the features.

  - `"NULL"`: No labels.

  - `"row.names"`: Labels will be extracted from the column names of the
    data matrix.

  The default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pca_loadings_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `pca_loadings_plot` object inherits the following `struct` classes:\
\
`[pca_loadings_plot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = pca_loadings_plot(
      components = c(1, 2),
      style = "points",
      label_features = NULL)

C = pca_loadings_plot()
```
