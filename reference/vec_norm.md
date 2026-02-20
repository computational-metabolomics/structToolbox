# Vector normalisation

The samples in the data matrix are normalised to account for differences
in concentration by scaling each sample such that the sum of squares is
equal to 1.

## Usage

``` r
vec_norm(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `vec_norm` object with the following `output` slots:

|  |  |
|----|----|
| `normalised` | (DatasetExperiment) A DatasetExperiment object containing the normalised data. |
| `coeff` | (data.frame) The normalisation coefficients calculated by PQN. |

struct object

## Inheritance

A `vec_norm` object inherits the following `struct` classes:\
\
`[vec_norm]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = vec_norm()

M = vec_norm()
```
