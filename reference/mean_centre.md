# Mean centre

The mean sample is subtracted from all samples in the data matrix. The
features in the centred matrix all have zero mean.

## Usage

``` r
mean_centre(mode = "data", ...)
```

## Arguments

- mode:

  (character) Mode of action. Allowed values are limited to the
  following:

  - `"data"`: Centring is applied to the data block.

  - `"sample_meta"`: Centring is applied to the sample_meta block.

  - `"both"`: Centring is applied to both the data and the sample_meta
    blocks.

  The default is `"data"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `mean_centre` object with the following `output` slots:

|                    |                     |
|--------------------|---------------------|
| `centred`          | (DatasetExperiment) |
| `mean_data`        | (numeric)           |
| `mean_sample_meta` | (numeric)           |

## Inheritance

A `mean_centre` object inherits the following `struct` classes:\
\
`[mean_centre]` \>\> `[preprocess]` \>\> `[model]` \>\> `[struct_class]`

## Examples

``` r
M = mean_centre(
      mode = "data")

M = mean_centre()
```
