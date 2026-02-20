# PLSDA feature importance summary plot

A plot of the selected feature significance metric for a PLSDA model for
the top selected features.

## Usage

``` r
plsda_feature_importance_plot(n_features = 30, metric = "vip", ...)
```

## Arguments

- n_features:

  (numeric, integer) The number of features to include in the summary.
  The default is `30`.\

- metric:

  (character) Metric to plot. Allowed values are limited to the
  following:

  - `"sr"`: Plot Selectivity Ratio.

  - `"sr_pvalue"`: Plot SR p-values.

  - `"vip"`: Plot Variable Importance in Projection scores.

  The default is `"vip"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` plsda_feature_importance_plot ` object. This object has no `output`
slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `pls`

- `ggplot2`

- `reshape2`

- `cowplot`

## Inheritance

A `plsda_feature_importance_plot` object inherits the following `struct`
classes:\
\
`[plsda_feature_importance_plot]` \>\> `[chart]` \>\> `[struct_class]`

## References

Liland K, Mevik B, Wehrens R (2024). *pls: Partial Least Squares and
Principal Component Regression*. doi:10.32614/CRAN.package.pls
<https://doi.org/10.32614/CRAN.package.pls>, R package version 2.8-5,
<https://CRAN.R-project.org/package=pls>.

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. ISBN 978-3-319-24277-4,
<https://ggplot2.tidyverse.org>.

Wickham H (2007). "Reshaping Data with the reshape Package." *Journal of
Statistical Software*, *21*(12), 1-20.
<https://www.jstatsoft.org/v21/i12/>.

Wilke C (2025). *cowplot: Streamlined Plot Theme and Plot Annotations
for 'ggplot2'*. doi:10.32614/CRAN.package.cowplot
<https://doi.org/10.32614/CRAN.package.cowplot>, R package version
1.2.0, <https://CRAN.R-project.org/package=cowplot>.

## Examples

``` r
M = plsda_feature_importance_plot(
      n_features = 50,
      metric = "vip")

D = iris_DatasetExperiment()
M = mean_centre()+PLSDA(factor_name='Species')
M = model_apply(M,D)

C = plsda_feature_importance_plot(n_features=30,metric='vip')
chart_plot(C,M[2])
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the structToolbox package.
#>   Please report the issue to the authors.
```
