# Calculate metric

Calculate metric

## Usage

``` r
# S4 method for class 'AUC'
calculate(obj, Y, Yhat)

# S4 method for class 'balanced_accuracy'
calculate(obj, Y, Yhat)

# S4 method for class 'balanced_error'
calculate(obj, Y, Yhat)

# S4 method for class 'r_squared'
calculate(obj, Y, Yhat)
```

## Arguments

- obj:

  a metric object

- Y:

  the true values/group labels

- Yhat:

  the predicted values/group labels

## Value

a modified metric object

## Examples

``` r
MET = metric()
calculate(MET)
#> Warning: no calculation provided for this metric
#> A "metric" object
#> -----------------
#> name:          
#> description:   
#> value:         
#> 
```
