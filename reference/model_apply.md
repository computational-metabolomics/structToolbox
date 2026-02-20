# Apply method

Applies method to the input DatasetExperiment

## Usage

``` r
# S4 method for class 'ANOVA,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'HSD,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'mixed_effect,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'HSDEM,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'classical_lsq,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'confounders_clsq,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'constant_sum_norm,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'corr_coef,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'split_data,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'equal_split,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'filter_smeta,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'fisher_exact,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'fold_change,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'fold_change_int,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'HCA,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'knn_impute,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'kw_rank_sum,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'log_transform,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'mean_of_medians,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'nroot_transform,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'pairs_filter,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'prop_na,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'rsd_filter,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'sb_corr,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'stratified_split,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'tSNE,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'ttest,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'vec_norm,DatasetExperiment'
model_apply(M, D)

# S4 method for class 'wilcox_test,DatasetExperiment'
model_apply(M, D)
```

## Arguments

- M:

  a method object

- D:

  another object used by the first

## Value

Returns a modified method object

## Examples

``` r
M=model()
model_apply(M,DatasetExperiment())
#> Warning: no training implemented for this model
#> A "model" object
#> ----------------
#> name:          
#> description:   
#> predicted:     
#> seq_in:        data
#> 
```
