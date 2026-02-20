# tSNE

t-Distributed Stochastic Neighbor Embedding.

## Usage

``` r
tSNE(
  dims = 2,
  perplexity = 30,
  max_iter = 100,
  theta = 0.5,
  check_duplicates = FALSE,
  init = NULL,
  eta = 200,
  ...
)
```

## Arguments

- dims:

  (numeric) The number of tSNE dimensions computed. The default is `2`.\

- perplexity:

  (numeric) Perplexity parameter. The default is `30`.\

- max_iter:

  (numeric) The maximum number of tSNE iterations. The default is
  `100`.\

- theta:

  (numeric) Speed/accuracy trade-off. A value of 0 gives an exact tSNE.
  The default is `0.5`.\

- check_duplicates:

  (logical) Check for duplicates. Allowed values are limited to the
  following:

  - `"TRUE"`: Checks for the presence of exact duplicate samples.

  - `"FALSE"`: Does not check for exact duplicate samples.

  The default is `FALSE`.\

- init:

  (NULL, data.frame, DatasetExperiment) A set of coordinates for
  initialising the tSNE algorithm. NULL uses random initialisation. The
  default is `NULL`.

- eta:

  (numeric) The learning rate parameter. The default is `200`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `tSNE` object with the following `output` slots:

|     |                     |
|-----|---------------------|
| `Y` | (DatasetExperiment) |

## Details

This object makes use of functionality from the following packages:

- `Rtsne`

## Inheritance

A `tSNE` object inherits the following `struct` classes:\
\
`[tSNE]` \>\> `[model]` \>\> `[struct_class]`

## References

Krijthe JH (2015). *Rtsne: T-Distributed Stochastic Neighbor Embedding
using Barnes-Hut Implementation*. R package version 0.17,
<https://github.com/jkrijthe/Rtsne>.

van der Maaten L, Hinton G (2008). "Visualizing High-Dimensional Data
Using t-SNE." *Journal of Machine Learning Research*, *9*, 2579-2605.

van der Maaten L (2014). "Accelerating t-SNE using Tree-Based
Algorithms." *Journal of Machine Learning Research*, *15*, 3221-3245.

## Examples

``` r
M = tSNE(
      dims = 2,
      perplexity = 30,
      max_iter = 1000,
      theta = 0.5,
      check_duplicates = FALSE,
      init = NULL,
      eta = 200)

M = tSNE()
```
