# permutation_test_plot class

Plots the results of a permutation test.

## Usage

``` r
permutation_test_plot(style = "boxplot", binwidth = 0.05, ...)
```

## Arguments

- style:

  The plot style. One of 'boxplot', 'violin', 'histogram', 'density' or
  'scatter'.

- binwidth:

  Binwidth for the "histogram" style. Ignored for all other styles.

- ...:

  additional slots and values passed to struct_class

## Value

struct object

## Examples

``` r
C = permutation_test_plot(style='boxplot')
```
