## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.align = 'center'
)
library(structToolbox)
library(gridExtra)

## ------------------------------------------------------------------------
D = sbcms_dataset()
summary(D)

## ------------------------------------------------------------------------
# peak matrix processing
PMP = pqn_norm(qc_label = 'QC',factor_name='class') + 
      knn_impute(neighbours=5) + 
      glog_transform(qc_label='QC',factor_name='class')
PMP = method.apply(PMP,D)

# multivariate analysis
MVA = mean_centre() + PCA(number_components = 10)
MVA = model.train(MVA,predicted(PMP))
MVA = model.predict(MVA,predicted(PMP))

# charts
C = pca_scores_plot(factor_name = 'Class',groups=D$sample_meta$class)
chart.plot(C,MVA[2])

## ------------------------------------------------------------------------
# initialise the method object
M = filter_na_count(factor_name='batch')

# apply the method
M = method.apply(M,D)

# get the counts
head(M$count)

# change the factor
M$factor_name='class'

# apply the method again
M = method.apply(M,D)

# get the counts
head(M$count)

## ------------------------------------------------------------------------
# method sequence
     
MS = # rsd filter
     rsd_filter(rsd_threshold=20,qc_label='QC',factor_name='class') +
     # wilcox test
     wilcox_test(factor_names='sample_type',alpha = 1e-14, predicted='significant') + 
     filter_by_name(mode='exclude',dimension='variable',seq_in = 'names') +
     # remove QCs
     filter_smeta(mode='exclude',levels='QC',factor_name='class') +
     # kruskal wallis
     kw_rank_sum(alpha=1e-4,factor_names='batch',predicted = 'significant') +
     filter_by_name(mode='exclude',dimension='variable',seq_in = 'names')

# apply the sequence
MS = method.apply(MS,D)

## ------------------------------------------------------------------------
# get the feature ids to keep
to_filter = colnames(predicted(MS)$data)

# new method sequence
MS2 = # filter by id
     filter_by_name(mode='include',dimension='variable',names=to_filter) +
     # pqn normalised
     pqn_norm(qc_label = 'QC',factor_name='class') + 
     # impute missing values
     knn_impute(neighbours=5) + 
     # scale
     glog_transform(qc_label='QC',factor_name='class')

# apply the method sequence
MS2 = method.apply(MS2,D)

## ------------------------------------------------------------------------
# multivariate analysis
MVA2 = mean_centre() + PCA(number_components = 10)
MVA2 = model.train(MVA2,predicted(MS2))
MVA2 = model.predict(MVA2,predicted(MS2))

# charts
C = pca_scores_plot(factor_name = 'Class',groups=D$sample_meta$class)
chart.plot(C,MVA2[2])

## ------------------------------------------------------------------------
C=feature_profile(run_order='sample_order',
                  qc_label='QC',
                  qc_column='class',
                  colour_by='batch',
                  feature_to_plot='X110.07126')
chart.plot(C,predicted(MS2))

## ------------------------------------------------------------------------
C=feature_profile(run_order='sample_order',
                  qc_label='QC',
                  qc_column='class',
                  colour_by='batch',
                  feature_to_plot='X486.32747')
chart.plot(C,predicted(PMP))

## ------------------------------------------------------------------------
C=feature_profile(run_order='sample_order',
                  qc_label='QC',
                  qc_column='class',
                  colour_by='batch',
                  feature_to_plot='X556.37036')
chart.plot(C,predicted(PMP))

## ------------------------------------------------------------------------
# names of charts for wilcoxon method
chart.names(wilcox_test())

# create object
C = wilcox_p_hist()

# plot
chart.plot(C,MS[2])

## ------------------------------------------------------------------------
# names of charts for wilcoxon method
chart.names(kw_rank_sum())

# create object
C = kw_p_hist()

# plot
chart.plot(C,MS[5])

