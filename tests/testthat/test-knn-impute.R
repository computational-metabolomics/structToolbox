# knn impute
test_that('knn impute',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[5,1]=NA
  # method
  M = knn_impute(neighbours=5,by = 'features')
  # apply
  M = model_apply(M,D)
  # expect_equal(M$imputed$data[5,1],5.2,tolerance=0.001) # an imputed value
  expect_equal(M$imputed$data[5,1],1.12,tolerance=0.001) # an imputed value
})
