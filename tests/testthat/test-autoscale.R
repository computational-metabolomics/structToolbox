# autoscale
test_that('autoscale',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = autoscale()
  # apply
  M = model_train(M,D)
  # predict
  M = model_predict(M,D)
  expect_equal(M$autoscaled$data[1,1],-0.897,tolerance=0.001)
})
