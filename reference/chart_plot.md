# chart_plot method

Plots a chart object

## Usage

``` r
# S4 method for class 'dfa_scores_plot,DFA'
chart_plot(obj, dobj)

# S4 method for class 'scatter_chart,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'pca_correlation_plot,PCA'
chart_plot(obj, dobj)

# S4 method for class 'pca_scores_plot,PCA'
chart_plot(obj, dobj)

# S4 method for class 'pca_biplot,PCA'
chart_plot(obj, dobj)

# S4 method for class 'pca_loadings_plot,PCA'
chart_plot(obj, dobj)

# S4 method for class 'pca_scree_plot,PCA'
chart_plot(obj, dobj)

# S4 method for class 'pca_dstat_plot,PCA'
chart_plot(obj, dobj)

# S4 method for class 'plsr_prediction_plot,PLSR'
chart_plot(obj, dobj)

# S4 method for class 'plsr_residual_hist,PLSR'
chart_plot(obj, dobj)

# S4 method for class 'plsr_qq_plot,PLSR'
chart_plot(obj, dobj)

# S4 method for class 'plsr_cook_dist,PLSR'
chart_plot(obj, dobj)

# S4 method for class 'pls_scores_plot,PLSR'
chart_plot(obj, dobj)

# S4 method for class 'plsda_predicted_plot,PLSDA'
chart_plot(obj, dobj)

# S4 method for class 'plsda_roc_plot,PLSDA'
chart_plot(obj, dobj)

# S4 method for class 'pls_vip_plot,PLSR'
chart_plot(obj, dobj)

# S4 method for class 'pls_regcoeff_plot,PLSR'
chart_plot(obj, dobj)

# S4 method for class 'blank_filter_hist,blank_filter'
chart_plot(obj, dobj)

# S4 method for class 'confounders_lsq_barchart,confounders_clsq'
chart_plot(obj, dobj)

# S4 method for class 'confounders_lsq_boxplot,confounders_clsq'
chart_plot(obj, dobj)

# S4 method for class 'feature_boxplot,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'mv_histogram,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'mv_boxplot,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'DatasetExperiment_dist,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'DatasetExperiment_boxplot,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'compare_dist,DatasetExperiment'
chart_plot(obj, dobj, eobj)

# S4 method for class 'DatasetExperiment_heatmap,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'DatasetExperiment_factor_boxplot,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'feature_profile_array,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'feature_profile,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'fold_change_plot,fold_change'
chart_plot(obj, dobj)

# S4 method for class 'fs_line,forward_selection_by_rank'
chart_plot(obj, dobj)

# S4 method for class 'glog_opt_plot,glog_transform'
chart_plot(obj, dobj, gobj)

# S4 method for class 'gs_line,grid_search_1d'
chart_plot(obj, dobj)

# S4 method for class 'hca_dendrogram,HCA'
chart_plot(obj, dobj)

# S4 method for class 'kfoldxcv_grid,kfold_xval'
chart_plot(obj, dobj)

# S4 method for class 'kfoldxcv_metric,kfold_xval'
chart_plot(obj, dobj)

# S4 method for class 'kw_p_hist,kw_rank_sum'
chart_plot(obj, dobj)

# S4 method for class 'mv_feature_filter_hist,mv_feature_filter'
chart_plot(obj, dobj)

# S4 method for class 'mv_sample_filter_hist,mv_sample_filter'
chart_plot(obj, dobj)

# S4 method for class 'permutation_test_plot,permutation_test'
chart_plot(obj, dobj)

# S4 method for class 'plsda_feature_importance_plot,PLSDA'
chart_plot(obj, dobj)

# S4 method for class 'pqn_norm_hist,pqn_norm'
chart_plot(obj, dobj)

# S4 method for class 'resample_chart,resample'
chart_plot(obj, dobj)

# S4 method for class 'rsd_filter_hist,rsd_filter'
chart_plot(obj, dobj)

# S4 method for class 'feature_profile,sb_corr'
chart_plot(obj, dobj, gobj)

# S4 method for class 'svm_plot_2d,SVM'
chart_plot(obj, dobj, gobj)

# S4 method for class 'tSNE_scatter,tSNE'
chart_plot(obj, dobj)

# S4 method for class 'tic_chart,DatasetExperiment'
chart_plot(obj, dobj)

# S4 method for class 'wilcox_p_hist,wilcox_test'
chart_plot(obj, dobj)
```

## Arguments

- obj:

  a chart object

- dobj:

  a struct object

- eobj:

  a second DatasetExperiment object to compare with the first

- gobj:

  The DatasetExperiment object before signal correction was applied.

## Value

a plot object

## Examples

``` r
C = example_chart()
chart_plot(C,iris_DatasetExperiment())
#> Warning: no chart defined for "DatasetExperiment"
#> A "example_chart" object
#> ------------------------
#> name:          
#> description:   
```
