# Filter by name

Filter samples/variables by row/column name, index or logicals.

## Usage

``` r
filter_by_name(mode = "exclude", dimension = "sample", names, ...)
```

## Arguments

- mode:

  (character) The filtering mode controls whether samples/features are
  mode="included" or mode="excluded" based on their name. The default is
  `"exclude"`.

- dimension:

  (character) The filtering dimensions controls whether
  dimension="sample" or dimension="variable" are filtered based on their
  name. The default is `"sample"`.

- names:

  (character, numeric, logical) The name of features/samples to be
  filtered. Must be an exact match. Can also provide indexes (numeric)
  or logical.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_by_name` object with the following `output` slots:

|            |                     |
|------------|---------------------|
| `filtered` | (DatasetExperiment) |

## Inheritance

A `filter_by_name` object inherits the following `struct` classes:\
\
`[filter_by_name]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = filter_by_name(
      mode = "exclude",
      dimension = "sample",
      names = character(0))

D = MTBLS79_DatasetExperiment()
M = filter_by_name(mode='exclude',dimension='variable',names=c(1,2,3))
M = model_apply(M,D)
```
