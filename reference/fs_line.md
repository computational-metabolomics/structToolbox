# Forward selection line plot

A line plot for forward selection. The computed model performance metric
is plotted against the number of features included in the model.

## Usage

``` r
fs_line(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` fs_line ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Inheritance

A `fs_line` object inherits the following `struct` classes:\
\
`[fs_line]` \>\> `[chart]` \>\> `[struct_class]`

## Examples

``` r
M = fs_line()

# some data
D = MTBLS79_DatasetExperiment(filtered=TRUE)

# normalise, impute and scale then remove QCs
P = pqn_norm(qc_label='QC',factor_name='Class') +
    knn_impute(neighbours=5) +
    glog_transform(qc_label='QC',factor_name='Class') +
    filter_smeta(mode='exclude',levels='QC',factor_name='Class')
P = model_apply(P,D)
D = predicted(P)

# forward selection using a PLSDA model
M = forward_selection_by_rank(factor_name='Class',
                             min_no_vars=2,
                             max_no_vars=11,
                             variable_rank=1:2063) *
    (mean_centre() + PLSDA(number_components=1,
                           factor_name='Class'))
M = run(M,D,balanced_accuracy())

# chart
C = fs_line()
chart_plot(C,M)

```
