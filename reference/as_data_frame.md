# Convert to data.frame

Convert the outputs of the input model into a data.frame.

## Usage

``` r
# S4 method for class 'filter_na_count'
as_data_frame(M)

# S4 method for class 'ttest'
as_data_frame(M)

# S4 method for class 'wilcox_test'
as_data_frame(M)
```

## Arguments

- M:

  a model object

## Value

A data.frame of model outputs

## Examples

``` r
D = iris_DatasetExperiment()
M = filter_na_count(threshold=50,factor_name='Species')
M= model_apply(M,D)
df = as_data_frame(M)
```
