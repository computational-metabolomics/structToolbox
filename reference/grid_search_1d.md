# One dimensional grid search

A one dimensional grid search calculates a performance metric for a
model at evenly spaced values for a model input parameter. The "optimum"
value for the parameter is suggested as the one which maximises
performance, or minimises error (whichever is appropriate for the chosen
metric)

## Usage

``` r
grid_search_1d(
  param_to_optimise,
  search_values,
  model_index,
  factor_name,
  max_min = "min",
  ...
)
```

## Arguments

- param_to_optimise:

  (character) The name of the model input parameter that is the focus of
  the search.

- search_values:

  (ANY) The values of the input parameter being optimised.

- model_index:

  (numeric, integer) The index of the model in the sequence that uses
  the parameter being optimised.

- factor_name:

  (character) The name of a sample-meta column to use.

- max_min:

  (character) Maximise or minimise. Allowed values are limited to the
  following:

  - `"max"`: The optimium parameter value is suggested based on
    maximising the performance metric.

  - `"min"`: The optimium parameter value is suggested based on
    minimising the performance metric.

  The default is `"min"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `grid_search_1d` object with the following `output` slots:

|                 |              |
|-----------------|--------------|
| `results`       | (data.frame) |
| `metric`        | (data.frame) |
| `optimum_value` | (numeric)    |

## Inheritance

A `grid_search_1d` object inherits the following `struct` classes:\
\
`[grid_search_1d]` \>\> `[resampler]` \>\> `[iterator]` \>\>
`[struct_class]`

## Examples

``` r
M = grid_search_1d(
      param_to_optimise = character(0),
      search_values = numeric(0),
      model_index = numeric(0),
      factor_name = "V1",
      max_min = "min")

D = MTBLS79_DatasetExperiment()
# some preprocessing
M = pqn_norm(qc_label='QC',factor_name='Class') +
    knn_impute() +
    glog_transform(qc_label='QC',factor_name='Class') +
    filter_smeta(factor_name='Class',levels='QC',mode='exclude')
M=model_apply(M,D)
D=predicted(M)

# reduce number of features for this example
D=D[,1:10]

# optmise number of components for PLS model
I = grid_search_1d(param_to_optimise='number_components',search_values=1:5,
        model_index=2,factor_name='Class') *
        (mean_centre()+PLSDA(factor_name='Class'))
I = run(I,D,balanced_accuracy())
```
