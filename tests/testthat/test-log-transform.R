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

test_that('log_transform train then predict',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # reference
  A = model_apply(log_transform(),D)
  # train then predict on its own
  M = model_train(log_transform(),D)
  M = model_predict(M,D)
  expect_equal(M$transformed$data,A$transformed$data)
  # train then predict in a model sequence
  S = log_transform() + mean_centre()
  S = model_train(S,D)
  S = model_predict(S,D)
  expect_equal(S[1]$transformed$data,A$transformed$data)
  expect_equal(dim(predicted(S)),dim(D))
})
