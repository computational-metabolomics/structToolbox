# Normalisation to constant sum

Each sample is normalised such that the total signal is equal to one (or
a scaling factor if specified).

## Usage

``` r
constant_sum_norm(scaling_factor = 1, ...)
```

## Arguments

- scaling_factor:

  (numeric) The scaling factor applied after normalisation. The default
  is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `constant_sum_norm` object with the following `output` slots:

|  |  |
|----|----|
| `normalised` | (DatasetExperiment) A DatasetExperiment object containing the normalised data. |
| `coeff` | (data.frame) The sum of each row, used to normalise the samples. |

## Inheritance

A `constant_sum_norm` object inherits the following `struct` classes:\
\
`[constant_sum_norm]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = constant_sum_norm(
      scaling_factor = 1)

M = constant_sum_norm()
```
