# pls_regcoeff_plot class

Plots the regression coefficients of a PLSDA model.

Plots the regression coefficient scores of a PLSDA model

## Usage

``` r
pls_regcoeff_plot(ycol = 1, ...)
```

## Arguments

- ycol:

  (character, numeric, integer) The Y column to plot. The default is
  `1`.\

- ...:

  additional slots and values passed to struct_class

## Value

A ` pls_regcoeff_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

struct object

## Details

This object makes use of functionality from the following packages:

- `pls`

- `ggplot2`

## Inheritance

A `pls_regcoeff_plot` object inherits the following `struct` classes:\
\
`[pls_regcoeff_plot]` \>\> `[chart]` \>\> `[struct_class]`

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
M = pls_regcoeff_plot(
      ycol = 1)

D = iris_DatasetExperiment()
M = mean_centre()+PLSDA(factor_name='Species')
M = model_apply(M,D)

C = pls_regcoeff_plot(ycol='setosa')
chart_plot(C,M[2])
```
