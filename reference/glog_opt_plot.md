# Glog optimisation

A plot of the sum of squares error (SSE) vs different values of lambda
for the glog transform. The indicated optimum value for lambda minimises
the SSE.

## Usage

``` r
glog_opt_plot(plot_grid = 100, ...)
```

## Arguments

- plot_grid:

  (numeric) The default is `100`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` glog_opt_plot ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `pmp`

## Inheritance

A `glog_opt_plot` object inherits the following `struct` classes:\
\
`[glog_opt_plot]` \>\> `[chart]` \>\> `[struct_class]`

## References

Jankevics A, Lloyd GR, Weber RJM (2025). *pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets*.
doi:10.18129/B9.bioc.pmp <https://doi.org/10.18129/B9.bioc.pmp>, R
package version 1.22.1, <https://bioconductor.org/packages/pmp>.

## Examples

``` r
M = glog_opt_plot(
      plot_grid = numeric(0))

D = iris_DatasetExperiment()
M = glog_transform(qc_label='versicolor',factor_name='Species')
M = model_apply(M,D)
#> Error!Lambda tending to infinity! Using standard
#> Error!Lambda tending to infinity! Using standard
C = glog_opt_plot()
chart_plot(C,M,D)
```
