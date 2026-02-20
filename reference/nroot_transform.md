# nth root transform

All values in the data matrix are transformed by raising them to the
power of 1/n.

## Usage

``` r
nroot_transform(root = 2, ...)
```

## Arguments

- root:

  (numeric) The nth root used for the transform. The default is `2`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `nroot_transform` object with the following `output` slots:

|  |  |
|----|----|
| `transformed` | (DatasetExperiment) A DatasetExperiment object containing the nth root transformed data. |

## Inheritance

A `nroot_transform` object inherits the following `struct` classes:\
\
`[nroot_transform]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = nroot_transform(
      root = 2)

M = nroot_transform()
```
