# Permutation test

A permutation test generates a "null" model by randomising the response
(for regression models) or group labels (for classification models).
This is repeated many times to generate a distribution of performance
metrics for the null model. This distribution can then be compared to
the performance of the true model. If there is overlap between the true
and null model performances then the model is overfitted.

## Usage

``` r
permutation_test(number_of_permutations = 50, factor_name, ...)
```

## Arguments

- number_of_permutations:

  (numeric, integer) The number of permutations. The default is `50`.\

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `permutation_test` object with the following `output` slots:

|                      |              |
|----------------------|--------------|
| `results.permuted`   | (data.frame) |
| `results.unpermuted` | (data.frame) |
| `metric`             | (data.frame) |

## Inheritance

A `permutation_test` object inherits the following `struct` classes:\
\
`[permutation_test]` \>\> `[resampler]` \>\> `[iterator]` \>\>
`[struct_class]`

## Examples

``` r
M = permutation_test(
      number_of_permutations = 100,
      factor_name = "V1")

I=permutation_test(factor_name='Species')
```
