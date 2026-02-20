# Feature boxplot

plots the new representation of data after applying tSNE.

## Usage

``` r
tSNE_scatter(factor_name, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` tSNE_scatter ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `Rtsne`

## Inheritance

A `tSNE_scatter` object inherits the following `struct` classes:\
\
`[tSNE_scatter]` \>\> `[chart]` \>\> `[struct_class]`

## References

Krijthe JH (2015). *Rtsne: T-Distributed Stochastic Neighbor Embedding
using Barnes-Hut Implementation*. R package version 0.17,
<https://github.com/jkrijthe/Rtsne>.

van der Maaten L, Hinton G (2008). "Visualizing High-Dimensional Data
Using t-SNE." *Journal of Machine Learning Research*, *9*, 2579-2605.

van der Maaten L (2014). "Accelerating t-SNE using Tree-Based
Algorithms." *Journal of Machine Learning Research*, *15*, 3221-3245.

## Examples

``` r
M = tSNE_scatter(
      factor_name = "V1")

M = tSNE_scatter(factor_name='Species')
```
