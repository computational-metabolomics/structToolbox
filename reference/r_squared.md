# Coefficient of determination (R-squared)

R-squared is a metric used to assess the goodness of fit for regression
models. It measures how much variance of one variable can be explained
by another variable.

## Usage

``` r
r_squared(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` r_squared ` object. This object has no `output` slots.

## Inheritance

A `r_squared` object inherits the following `struct` classes:\
\
`[r_squared]` \>\> `[metric]` \>\> `[struct_class]`

## Examples

``` r
M = r_squared()

MET = r_squared()
```
