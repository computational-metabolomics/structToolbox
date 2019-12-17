# filter by missing values per sample
test_that('mv_sample_filter',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[1:3,]=NA
  # method
  M = mv_sample_filter(mv_threshold=50)
  # apply
  M=model_apply(M,D)
  expect_equal(nrow(M$filtered$data),147)
})

test_that('mv_sample_filter plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[1:3,]=NA
  # method
  M = mv_sample_filter(mv_threshold=50)
  # apply
  M=model_apply(M,D)
  # chart
  C = mv_sample_filter_hist()
  gg=chart_plot(C,M)
  expect_true(is(gg,'ggplot'))
})
