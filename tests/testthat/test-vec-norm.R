# vecnorm
test_that('vecnorm',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = vec_norm()
  # apply
  M = model_apply(M,D)
  expect_equal(sum(M$normalised$data[1,]^2),1,tolerance=0.00001)
  expect_equal(sum(M$normalised$data[5,]^2),1,tolerance=0.00001)
  expect_equal(sum(M$normalised$data[25,]^2),1,tolerance=0.00001)
  expect_equal(predicted(M)$data[1,1],0.8037,tolerance=0.0001)
})
