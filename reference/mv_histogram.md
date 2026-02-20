# Missing value histogram

A histogram of the numbers of missing values per sample/feature

## Usage

``` r
mv_histogram(label_outliers = TRUE, by_sample = TRUE, ...)
```

## Arguments

- label_outliers:

  (logical) Label outliers. Allowed values are limited to the following:

  - `"TRUE"`: Sample labels for potential outliers are displayed on the
    plot.

  - `"FALSE"`: Sample labels are not included on the plot.

  The default is `TRUE`.\

- by_sample:

  (logical) Plot by sample or by feature. Allowed values are limited to
  the following:

  - `"TRUE"`: Missing values are plotted per sample.

  - `"FALSE"`: Missing values are plotted per feature.

  The default is `TRUE`.\

- ...:

  additional slots and values passed to struct_class

## Value

A ` mv_histogram ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

struct object

## Inheritance

A `mv_histogram` object inherits the following `struct` classes:\
\
`[mv_histogram]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = mv_histogram(
      label_outliers = FALSE,
      by_sample = FALSE)

D = MTBLS79_DatasetExperiment()
C = mv_histogram(label_outliers=FALSE,by_sample=FALSE)
chart_plot(C,D)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_bar()`).

```
