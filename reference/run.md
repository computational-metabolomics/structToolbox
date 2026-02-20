# Runs an iterator, applying the chosen model multiple times.

Running an iterator will apply the iterator a number of times to a
DatasetExperiment. For example, in cross-validation the same model is
applied multiple times to the same data, splitting it into training and
test sets. The input metric object can be calculated and collected for
each iteration as an output.

## Usage

``` r
# S4 method for class 'bootstrap,DatasetExperiment,metric'
run(I, D, MET = NULL)

# S4 method for class 'forward_selection_by_rank,DatasetExperiment,metric'
run(I, D, MET)

# S4 method for class 'grid_search_1d,DatasetExperiment,metric'
run(I, D, MET)

# S4 method for class 'kfold_xval,DatasetExperiment,metric'
run(I, D, MET = NULL)

# S4 method for class 'permutation_test,DatasetExperiment,metric'
run(I, D, MET = NULL)

# S4 method for class 'permute_sample_order,DatasetExperiment,metric'
run(I, D, MET)

# S4 method for class 'resample,DatasetExperiment,metric'
run(I, D, MET)
```

## Arguments

- I:

  an iterator object

- D:

  a DatasetExperiment object

- MET:

  a metric object

## Value

Modified iterator object

## Examples

``` r
D = iris_DatasetExperiment() # get some data
MET = metric()  # use a metric
I = example_iterator() # initialise iterator
models(I) = example_model() # set the model
I = run(I,D,MET) # run
#> Warning: no calculation provided for this metric
```
