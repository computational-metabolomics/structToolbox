# Bootstrap resampling

In bootstrap resampling a subset of samples is selected at random with
replacement to form a training set. Any sample not selected for training
is included in the test set. This process is repeated many times, and
performance metrics are computed for each repetition.

## Usage

``` r
bootstrap(number_of_repetitions = 100, collect, ...)
```

## Arguments

- number_of_repetitions:

  (numeric, integer) The number of bootstrap repetitions. The default is
  `100`.\

- collect:

  (character) The name of a model output to collect over all bootstrap
  repetitions, in addition to the input metric.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `bootstrap` object with the following `output` slots:

|             |                 |
|-------------|-----------------|
| `results`   | (data.frame)    |
| `metric`    | (data.frame)    |
| `collected` | (logical, list) |

## Inheritance

A `bootstrap` object inherits the following `struct` classes:\
\
`[bootstrap]` \>\> `[resampler]` \>\> `[iterator]` \>\> `[struct_class]`

## Examples

``` r
M = bootstrap(
      number_of_repetitions = 10,
      collect = "vip")

I = bootstrap(number_of_repetitions = 10, collect = 'vip')
```
