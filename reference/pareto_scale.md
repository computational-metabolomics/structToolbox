# Pareto scaling

The mean sample is subtracted from all samples and then scaled by the
square root of the standard deviation. The transformed data has zero
mean.

## Usage

``` r
pareto_scale(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `pareto_scale` object with the following `output` slots:

|          |                     |
|----------|---------------------|
| `scaled` | (DatasetExperiment) |
| `mean`   | (numeric)           |
| `sd`     | (numeric)           |

## Inheritance

A `pareto_scale` object inherits the following `struct` classes:\
\
`[pareto_scale]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = pareto_scale()

D = iris_DatasetExperiment()
M = pareto_scale()
M = model_train(M,D)
M = model_predict(M,D)
```
