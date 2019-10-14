# split data class
test_that('split data',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = split_data()
  # apply
  M = model.apply(M,D)
  expect_equal(nrow(M$testing$data),38)
  expect_equal(nrow(M$training$data),112)
})
