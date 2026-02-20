# Discriminant Factor Analysis

Discriminant Factor Analysis (DFA) is a supervised classification
method. Using a linear combination of the input variables, DFA finds new
orthogonal axes (canonical values) to minimize the variance within each
given class and maximize variance between classes.

## Usage

``` r
DFA(factor_name, number_components = 2, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- number_components:

  (numeric, integer) The number of DFA components calculated. The
  default is `2`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `DFA` object with the following `output` slots:

|               |                     |
|---------------|---------------------|
| `scores`      | (DatasetExperiment) |
| `loadings`    | (data.frame)        |
| `eigenvalues` | (data.frame)        |
| `that`        | (DatasetExperiment) |

## Inheritance

A `DFA` object inherits the following `struct` classes:\
\
`[DFA]` \>\> `[model]` \>\> `[struct_class]`

## References

Manly B (1986). *Multivariate Statistical Methods: A Primer*. Chapman
and Hall, Boca Raton.

## Examples

``` r
M = DFA(
      factor_name = "V1",
      number_components = 2)

D = iris_DatasetExperiment()
M = DFA(factor_name='Species')
M = model_apply(M,D)
```
