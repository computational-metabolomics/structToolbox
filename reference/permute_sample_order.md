# Permute Sample Order

The order of samples in the data matrix is randomly permuted. The
relationship between the samples and the sample meta data is maintained.

## Usage

``` r
permute_sample_order(number_of_permutations = 10, ...)
```

## Arguments

- number_of_permutations:

  (numeric, integer) The number of times the sample order is permuted.
  The default is `10`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `permute_sample_order` object with the following `output` slots:

|                |              |
|----------------|--------------|
| `results`      | (data.frame) |
| `metric`       | (data.frame) |
| `metric.train` | (numeric)    |

## Inheritance

A `permute_sample_order` object inherits the following `struct`
classes:\
\
`[permute_sample_order]` \>\> `[resampler]` \>\> `[iterator]` \>\>
`[struct_class]`

## Examples

``` r
M = permute_sample_order(
      number_of_permutations = 100)

C = permute_sample_order()
```
