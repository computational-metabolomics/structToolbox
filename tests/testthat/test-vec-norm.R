# vecnorm
test_that('vecnorm',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = vec_norm()
  # apply
  M = method.apply(M,D)
  expect_equal(M$normalised$data[1,1],0.172,tolerance=0.001)
})
