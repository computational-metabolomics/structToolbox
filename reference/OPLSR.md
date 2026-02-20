# Orthogonal Partial Least Squares regression

OPLS splits a data matrix into two parts. One part contains information
orthogonal to the input vector, and the other is non-orthogonal.

## Usage

``` r
OPLSR(number_components = 2, factor_name, ...)
```

## Arguments

- number_components:

  (numeric, integer) The number of orthgonal components. The default is
  `2`.\

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `OPLSR` object with the following `output` slots:

|              |                     |
|--------------|---------------------|
| `opls_model` | (list)              |
| `filtered`   | (DatasetExperiment) |
| `orthogonal` | (DatasetExperiment) |

## Inheritance

A `OPLSR` object inherits the following `struct` classes:\
\
`[OPLSR]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = OPLSR(
      number_components = 2,
      factor_name = "V1")

M = OPLSR('number_components'=2,factor_name='Species')
```
