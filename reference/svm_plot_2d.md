# SVM scatter plot

A scatter plot of the input data by group and the calculated boundary of
a SVM model.

## Usage

``` r
svm_plot_2d(factor_name, npoints = 100, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- npoints:

  (numeric) The number of grid points used to plot the boundary. The
  default is `100`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` svm_plot_2d ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `e1071`

## Inheritance

A `svm_plot_2d` object inherits the following `struct` classes:\
\
`[svm_plot_2d]` \>\> `[chart]` \>\> `[struct_class]`

## References

Meyer D, Dimitriadou E, Hornik K, Weingessel A, Leisch F (2025). *e1071:
Misc Functions of the Department of Statistics, Probability Theory Group
(Formerly: E1071), TU Wien*. doi:10.32614/CRAN.package.e1071
<https://doi.org/10.32614/CRAN.package.e1071>, R package version 1.7-17,
<https://CRAN.R-project.org/package=e1071>.

## Examples

``` r
M = svm_plot_2d(
      factor_name = "V1",
      npoints = 100)

D = iris_DatasetExperiment()
M = filter_smeta(mode='exclude',levels='setosa',factor_name='Species') +
    mean_centre()+PCA(number_components=2)+
    SVM(factor_name='Species',kernel='linear')
M = model_apply(M,D)

C = svm_plot_2d(factor_name='Species')
chart_plot(C,M[4],predicted(M[3]))

```
