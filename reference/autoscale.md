# Autoscaling

Each variable/feature is mean centred and scaled by the standard
deviation. The transformed variables have zero-mean and unit-variance.

## Usage

``` r
autoscale(mode = "data", ...)
```

## Arguments

- mode:

  (character) Mode of action. Allowed values are limited to the
  following:

  - `"data"`: Autoscaling is applied to the data matrix only.

  - `"sample_meta"`: Autoscaling is applied to the sample_meta data
    only.

  - `"both"`: Autoscaling is applied to both the data matrix and the
    meta data.

  The default is `"data"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `autoscale` object with the following `output` slots:

|                    |                     |
|--------------------|---------------------|
| `autoscaled`       | (DatasetExperiment) |
| `mean_data`        | (numeric)           |
| `sd_data`          | (numeric)           |
| `mean_sample_meta` | (numeric)           |
| `sd_sample_meta`   | (numeric)           |

## Inheritance

A `autoscale` object inherits the following `struct` classes:\
\
`[autoscale]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = autoscale(
      mode = "data")

D = iris_DatasetExperiment()
M = autoscale()
M = model_train(M,D)
M = model_predict(M,D)
```
