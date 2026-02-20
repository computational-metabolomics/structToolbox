# Feature distribution histogram

A boxplot to visualise the distribution of values within a subset of
features.

## Usage

``` r
DatasetExperiment_boxplot(
  factor_name,
  by_sample = TRUE,
  per_class = TRUE,
  number = 50,
  ...
)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- by_sample:

  (logical) Plot by sample. Allowed values are limited to the following:

  - `"TRUE"`: The data is plotted across features for a subset of
    samples.

  - `"FALSE"`: The data is plotted across samples for a subset of
    features.

  The default is `TRUE`.\

- per_class:

  (logical) Plot per class. Allowed values are limited to the following:

  - `"TRUE"`: The data is plotted for each class.

  - `"FALSE"`: The data is plotted for all samples.

  The default is `TRUE`.\

- number:

  (numeric, integer) The number of features/samples plotted. The default
  is `50`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` DatasetExperiment_boxplot ` object. This object has no `output`
slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

struct object

## Inheritance

A `DatasetExperiment_boxplot` object inherits the following `struct`
classes:\
\
`[DatasetExperiment_boxplot]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = DatasetExperiment_boxplot(
      factor_name = "V1",
      by_sample = FALSE,
      per_class = FALSE,
      number = 50)

D = MTBLS79_DatasetExperiment()
C = DatasetExperiment_boxplot(factor_name='Class',number=10,per_class=FALSE)
chart_plot(C,D)
#> Warning: Removed 585 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
```
