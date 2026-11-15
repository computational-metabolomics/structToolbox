# blank filter
test_that('blank filter',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = blank_filter(blank_label='versicolor',qc_label='virginica',fold_change=1,factor_name='Species')
  # apply
  M=model_apply(M,D)
  expect_true(all(M$flags$blank_flags==1))
  expect_true(all(M$flags$blank_fraction_flags==1))
})

test_that('blank filter histogram',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = blank_filter(blank_label='versicolor',qc_label='virginica',fold_change=1,factor_name='Species')
  # apply
  M=model_apply(M,D)
  # chart
  C = blank_filter_hist()
  gg=chart_plot(C,M)
  expect_true(is(gg,'ggplot'))
})

test_that('blank filter warns when labels match no samples',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # blank label not found (case-sensitive): nothing is removed
  M = blank_filter(blank_label='Versicolor',qc_label='virginica',fold_change=1,factor_name='Species')
  expect_warning(M<-model_apply(M,D),'no features will be removed')
  expect_equal(ncol(predicted(M)),ncol(D))
  # qc label not found: everything is removed
  M = blank_filter(blank_label='versicolor',qc_label='QC',fold_change=1,factor_name='Species')
  expect_warning(M<-model_apply(M,D),'all features will be removed')
  expect_equal(ncol(predicted(M)),0)
  # no warning when both labels are found
  M = blank_filter(blank_label='versicolor',qc_label='virginica',fold_change=1,factor_name='Species')
  expect_no_warning(model_apply(M,D))
})

