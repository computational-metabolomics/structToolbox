# PLSDA predicted plot

A plot of the regression coefficients from a PLSDA model.

## Usage

``` r
plsda_predicted_plot(factor_name, style = "boxplot", ycol = 1, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- style:

  (character) Plot style. Allowed values are limited to the following:

  - `"boxplot"`: A boxplot.

  - `"violin"`: A violin plot.

  - `"density"`: A density plot.

  The default is `"boxplot"`.

- ycol:

  (character, numeric, integer) The column of the Y block to be plotted.
  The default is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` plsda_predicted_plot ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `pls`

- `ggplot2`

## Inheritance

A `plsda_predicted_plot` object inherits the following `struct`
classes:\
\
`[plsda_predicted_plot]` \>\> `[chart]` \>\> `[struct_class]`

## References

Liland K, Mevik B, Wehrens R (2024). *pls: Partial Least Squares and
Principal Component Regression*. doi:10.32614/CRAN.package.pls
<https://doi.org/10.32614/CRAN.package.pls>, R package version 2.8-5,
<https://CRAN.R-project.org/package=pls>.

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. ISBN 978-3-319-24277-4,
<https://ggplot2.tidyverse.org>.

## Examples

``` r
M = plsda_predicted_plot(
      factor_name = "V1",
      style = "boxplot",
      ycol = 1)

D = iris_DatasetExperiment()
M = mean_centre()+PLSDA(factor_name='Species')
M = model_apply(M,D)

C = plsda_predicted_plot(factor_name='Species')
chart_plot(C,M[2])
```
