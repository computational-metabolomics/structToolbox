# rsd filter
test_that('rsd filter',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = rsd_filter(qc_label='virginica',factor_name='Species',rsd_threshold=100)
  # apply
  M=model_apply(M,D)
  expect_true(all(M$flags$rsd_flags==1))
})

test_that('blank filter histogram',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = rsd_filter(qc_label='virginica',factor_name='Species',rsd_threshold=100)
  # apply
  M=model_apply(M,D)
  # chart
  C = rsd_filter_hist()
  gg=chart_plot(C,M)
  expect_true(is(gg,'ggplot'))
})
