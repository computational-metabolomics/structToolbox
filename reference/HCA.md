# Hierarchical Cluster Analysis

Hierarchical Cluster Analysis is a numerical technique that uses
agglomerative clustering to identify clusters or groupings of samples.

## Usage

``` r
HCA(
  dist_method = "euclidean",
  cluster_method = "complete",
  minkowski_power = 2,
  factor_name,
  ...
)
```

## Arguments

- dist_method:

  (character) Distance measure. Allowed values are limited to the
  following:

  - `"euclidean"`: The euclidean distance (2 norm).

  - `"maximum"`: The maximum distance.

  - `"manhattan"`: The absolute distance (1 norm).

  - `"canberra"`: A weighted version of the mahattan distance.

  - `"minkowski"`: A generalisation of manhattan and euclidean distance
    to nth norm.

  The default is `"euclidean"`.

- cluster_method:

  (character) Agglomeration method. Allowed values are limited to the
  following:

  - `"ward.D"`: Ward clustering.

  - `"ward.D2"`: Ward clustering using sqaured distances.

  - `"single"`: Single linkage.

  - `"complete"`: Complete linkage.

  - `"average"`: Average linkage (UPGMA).

  - `"mcquitty"`: McQuitty linkage (WPGMA).

  - `"median"`: Median linkage (WPGMC).

  - `"centroid"`: Centroid linkage (UPGMC).

  The default is `"complete"`.

- minkowski_power:

  (numeric) The default is `2`.\

- factor_name:

  (character) The name of a sample-meta column to use.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `HCA` object with the following `output` slots:

|  |  |
|----|----|
| `dist_matrix` | (dist) An object containing pairwise distance information between samples. |
| `hclust` | (hclust) An object of class hclust which describes the tree produced by the clustering process. |
| `factor_df` | (data.frame) |

## Details

This object makes use of functionality from the following packages:

- `stats`

## Inheritance

A `HCA` object inherits the following `struct` classes:\
\
`[HCA]` \>\> `[model]` \>\> `[struct_class]`

## References

R Core Team (2025). *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing, Vienna, Austria.
<https://www.R-project.org/>.

## Examples

``` r
M = HCA(
      dist_method = "euclidean",
      cluster_method = "complete",
      minkowski_power = numeric(0),
      factor_name = "V1")

D = iris_DatasetExperiment()
M = HCA(factor_name='Species')
M = model_apply(M,D)
```
