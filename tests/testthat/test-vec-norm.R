# vecnorm
test_that('vecnorm',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = vec_norm()
  # apply
  M = model_apply(M,D)
  expect_equal(M$normalised$data[1,1],0.172,tolerance=0.001)
})
