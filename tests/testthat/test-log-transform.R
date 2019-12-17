# log
test_that('log',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = log_transform()
  # apply
  M = model_apply(M,D)
  expect_equal(M$transformed$data[1,1],0.707,tolerance=0.001)
})
