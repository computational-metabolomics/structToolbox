# PCA biplot

A scatter plot of the selected principal component scores overlaid with
the corresponding principal component loadings.

## Usage

``` r
pca_biplot(
  components = c(1, 2),
  points_to_label = "none",
  factor_name,
  scale_factor = 0.95,
  style = "points",
  label_features = FALSE,
  ...
)
```

## Arguments

- components:

  (numeric) The principal components used to generate the plot. The
  default is `c(1, 2)`.

- points_to_label:

  (character) points_to_label. Allowed values are limited to the
  following:

  - `"none"`: No samples are labelled on the plot.

  - `"all"`: All samples are labelled on the plot.

  - `"outliers"`: Potential outliers are labelled on the plot.

  The default is `"none"`.

- factor_name:

  (character) The name of a sample-meta column to use.

- scale_factor:

  (numeric) The scaling factor applied to the loadings. The default is
  `0.95`.\

- style:

  (character) Plot style. Allowed values are limited to the following:

  - `"points"`: Loadings and scores are plotted as a scatter plot.

  - `"arrows"`: The loadings are plotted as arrow vectors.

  The default is `"points"`.

- label_features:

  (logical) Add feature labels. Allowed values are limited to the
  following:

  - `"TRUE"`: Features are labelled.

  - `"FALSE"`: Features are not labelled.

  The default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pca_biplot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `pca_biplot` object inherits the following `struct` classes:\
\
`[pca_biplot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = pca_biplot(
      components = c(1, 2),
      points_to_label = "none",
      factor_name = "V1",
      scale_factor = 0.95,
      style = "points",
      label_features = FALSE)

C = pca_biplot(factor_name='Species')
```
