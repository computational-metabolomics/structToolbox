# Pairs filter

This filter is used for study designs with paired sampling to ensure
that measurements from the same source (e.g. patient) are represented in
all factor levels and interactions.

## Usage

``` r
pairs_filter(factor_name, sample_id, ...)
```

## Arguments

- factor_name:

  (character) The name of a sample-meta column to use.

- sample_id:

  (character) Name of sample meta column containing sample identifiers.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `pairs_filter` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (DatasetExperiment) A DatasetExperiment object after the filter has been applied. |
| `flags` | (data.frame) A data.frame indicating whether features were filtered from the DatasetExperiment. |

struct object

## Inheritance

A `pairs_filter` object inherits the following `struct` classes:\
\
`[pairs_filter]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = pairs_filter(
      factor_name = "V1",
      sample_id = "V1")

M=pairs_filter(factor_name='Class',sample_id='ids')
```
