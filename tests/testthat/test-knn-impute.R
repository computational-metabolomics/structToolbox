# knn impute
test_that('knn impute',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$data[5,1]=NA
  # method
  M = knn_impute(neighbours=5)
  # apply
  M = method.apply(M,D)
  # expect_equal(M$imputed$data[5,1],5.2,tolerance=0.001) # an imputed value
  expect_equal(M$imputed$data[5,1],1.12,tolerance=0.001) # an imputed value
})
