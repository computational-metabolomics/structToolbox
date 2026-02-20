# Confounding factor relative change barchart

A barchart of the relative change (delta) in regression coefficient when
potential confounding factors are included, and excluded, from the
model. Factors with a large delta are considered to be confounding
factors.

## Usage

``` r
confounders_lsq_barchart(feature_to_plot, threshold = 10, ...)
```

## Arguments

- feature_to_plot:

  (numeric, character, integer) The column name of the feature to be
  plotted.

- threshold:

  (numeric) A horizontal line is plotted to indicate the threshold. The
  default is `10`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` confounders_lsq_barchart ` object. This object has no `output`
slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `confounders_lsq_barchart` object inherits the following `struct`
classes:\
\
`[confounders_lsq_barchart]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = confounders_lsq_barchart(
      feature_to_plot = 1,
      threshold = 10)

D = MTBLS79_DatasetExperiment()
M = filter_by_name(mode='include',dimension='variable',
        names=colnames(D$data)[1:10]) + # first 10 features
    filter_smeta(mode='exclude',levels='QC',
        factor_name='Class') + # reduce to two group comparison
    confounders_clsq(factor_name = 'Class',
        confounding_factors=c('run_order','Batch'))
M = model_apply(M,D)
C = C=confounders_lsq_barchart(feature_to_plot=1,threshold=15)
chart_plot(C,M[3])
```
