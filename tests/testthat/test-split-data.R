# split data class
test_that('split data',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = split_data(p=0.75)
  # apply
  M = model_apply(M,D)
  expect_equal(nrow(M$testing$data),38)
  expect_equal(nrow(M$training$data),112)
})
