# rsd filter
test_that('rsd filter',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = rsd_filter(qc_label='virginica',factor_name='Species',rsd_threshold=100)
  # apply
  M=model.apply(M,D)
  expect_true(all(M$flags$rsd_flags==1))
})

test_that('blank filter histogram',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = rsd_filter(qc_label='virginica',factor_name='Species',rsd_threshold=100)
  # apply
  M=model.apply(M,D)
  # chart
  C = rsd_filter.hist()
  gg=chart.plot(C,M)
  expect_true(is(gg,'ggplot'))
})
