# Minimum number of measured values filter

The number of measured values is counted for each feature, and any
feature with less than a predefined minimum number of values in each
group is removed. If there are several factors, then the threshold is
applied so that the minimum number of samples is present for all
combinations (interactions) of groups.

## Usage

``` r
filter_na_count(threshold, factor_name, ...)
```

## Arguments

- threshold:

  (numeric) The minimum number of samples in each group/interaction.

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_na_count` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (DatasetExperiment) A DatasetExperiment object containing the filtered data. |
| `count` | (data.frame) The number of measured values in each group/interaction. |
| `na_count` | (data.frame) The number of missing values in each group/interaction. |
| `flags` | (data.frame) Flags to indicate which features were removed. |

## Inheritance

A `filter_na_count` object inherits the following `struct` classes:\
\
`[filter_na_count]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = filter_na_count(
      threshold = 2,
      factor_name = "V1")

D = MTBLS79_DatasetExperiment()
M = filter_na_count(threshold=3,factor_name='Class')
M = model_apply(M,D)
```
