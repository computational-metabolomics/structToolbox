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

test_that('knn impute by samples uses the right limits',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # 60% missing in feature 1, allowed by feature_max but not by sample_max
  D$data[1:90,1]=NA
  # method
  M = knn_impute(neighbours=5,by='samples',feature_max=80,sample_max=50)
  # apply (errors if the feature limit is applied to samples)
  M = model_apply(M,D)
  expect_false(any(is.na(M$imputed$data)))
})
