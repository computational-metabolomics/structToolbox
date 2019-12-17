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
