# Reverse preprocessing

Reverse the effect of a preprocessing step on a DatasetExperiment.

## Usage

``` r
# S4 method for class 'autoscale,DatasetExperiment'
model_reverse(M, D)

# S4 method for class 'mean_centre,DatasetExperiment'
model_reverse(M, D)
```

## Arguments

- M:

  a model object

- D:

  a DatasetExperiment object

## Value

Returns a modified DatasetExperiment object

## Examples

``` r
M = example_model()
D = model_reverse(M,iris_DatasetExperiment())
```
