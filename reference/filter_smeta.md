# Filter by sample meta data

The data is filtered by so that the named levels of a factor are
included/excluded from the dataset.

## Usage

``` r
filter_smeta(mode = "include", levels, factor_name, ...)
```

## Arguments

- mode:

  (character) Mode of action. Allowed values are limited to the
  following:

  - `"include"`: Samples in the specified levels are retained.

  - `"exclude"`: Samples in the specified levels are excluded.

  The default is `"include"`.

- levels:

  (character) The level name(s) for filtering.

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_smeta` object with the following `output` slots:

|            |                     |
|------------|---------------------|
| `filtered` | (DatasetExperiment) |

## Inheritance

A `filter_smeta` object inherits the following `struct` classes:\
\
`[filter_smeta]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = filter_smeta(
      mode = "include",
      levels = character(0),
      factor_name = "V1")

D = MTBLS79_DatasetExperiment()
M = filter_smeta(mode='exclude',levels='QC',factor_name='QC')
M = model_apply(M,D)
```
