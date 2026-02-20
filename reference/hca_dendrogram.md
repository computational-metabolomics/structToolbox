# HCA dendrogram

A dendrogram visualising the clustering by HCA.

## Usage

``` r
hca_dendrogram(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` hca_dendrogram ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `ggdendro`

## Inheritance

A `hca_dendrogram` object inherits the following `struct` classes:\
\
`[hca_dendrogram]` \>\> `[chart]` \>\> `[struct_class]`

## References

de Vries A, Ripley BD (2024). *ggdendro: Create Dendrograms and Tree
Diagrams Using 'ggplot2'*. doi:10.32614/CRAN.package.ggdendro
<https://doi.org/10.32614/CRAN.package.ggdendro>, R package version
0.2.0, <https://CRAN.R-project.org/package=ggdendro>.

## Examples

``` r
M = hca_dendrogram()

C = hca_dendrogram()
```
