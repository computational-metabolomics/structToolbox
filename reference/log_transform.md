# logarithm transform

A logarithmic transform is applied to all values in the data matrix.

## Usage

``` r
log_transform(base = 10, ...)
```

## Arguments

- base:

  (numeric) The base of the logarithm used for the transform. The
  default is `10`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `log_transform` object with the following `output` slots:

|  |  |
|----|----|
| `transformed` | (DatasetExperiment) A DatasetExperiment object containing the log transformed data. |

struct object

## Inheritance

A `log_transform` object inherits the following `struct` classes:\
\
`[log_transform]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = log_transform(
      base = 10)

M = log_transform()
```
