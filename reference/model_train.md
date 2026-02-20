# Train a model

Trains a model using the input DatasetExperiment

## Usage

``` r
# S4 method for class 'DFA,DatasetExperiment'
model_train(M, D)

# S4 method for class 'PCA,DatasetExperiment'
model_train(M, D)

# S4 method for class 'PLSR,DatasetExperiment'
model_train(M, D)

# S4 method for class 'PLSDA,DatasetExperiment'
model_train(M, D)

# S4 method for class 'autoscale,DatasetExperiment'
model_train(M, D)

# S4 method for class 'blank_filter,DatasetExperiment'
model_train(M, D)

# S4 method for class 'constant_sum_norm,DatasetExperiment'
model_train(M, D)

# S4 method for class 'dratio_filter,DatasetExperiment'
model_train(M, D)

# S4 method for class 'filter_by_name,DatasetExperiment'
model_train(M, D)

# S4 method for class 'filter_na_count,DatasetExperiment'
model_train(M, D)

# S4 method for class 'filter_smeta,DatasetExperiment'
model_train(M, D)

# S4 method for class 'glog_transform,DatasetExperiment'
model_train(M, D)

# S4 method for class 'linear_model,DatasetExperiment'
model_train(M, D)

# S4 method for class 'mean_centre,DatasetExperiment'
model_train(M, D)

# S4 method for class 'mv_feature_filter,DatasetExperiment'
model_train(M, D)

# S4 method for class 'mv_sample_filter,DatasetExperiment'
model_train(M, D)

# S4 method for class 'OPLSR,DatasetExperiment'
model_train(M, D)

# S4 method for class 'OPLSDA,DatasetExperiment'
model_train(M, D)

# S4 method for class 'pareto_scale,DatasetExperiment'
model_train(M, D)

# S4 method for class 'pqn_norm,DatasetExperiment'
model_train(M, D)

# S4 method for class 'SVM,DatasetExperiment'
model_train(M, D)

# S4 method for class 'vec_norm,DatasetExperiment'
model_train(M, D)
```

## Arguments

- M:

  a model object

- D:

  a DatasetExperiment object

## Value

Returns a modified model object

## Examples

``` r
M = example_model()
M = model_train(M,iris_DatasetExperiment())
```
