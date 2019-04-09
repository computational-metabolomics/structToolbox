# filter by missing values per sample
test_that('mv_sample_filter',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$data[1:3,]=NA
  # method
  M = mv_sample_filter(mv_threshold=50)
  # apply
  M=method.apply(M,D)
  expect_equal(nrow(M$filtered$data),147)
})

test_that('mv_sample_filter plot',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$data[1:3,]=NA
  # method
  M = mv_sample_filter(mv_threshold=50)
  # apply
  M=method.apply(M,D)
  # chart
  C = mv_sample_filter.hist()
  gg=chart.plot(C,M)
  expect_true(is(gg,'ggplot'))
})
