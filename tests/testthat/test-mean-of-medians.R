# mean_of_medians

test_that('mean_of_medians train then predict',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # reference
  A = model_apply(mean_of_medians(factor_name='Species'),D)
  # train then predict on its own
  M = model_train(mean_of_medians(factor_name='Species'),D)
  M = model_predict(M,D)
  expect_equal(M$transformed$data,A$transformed$data)
  # train then predict in a model sequence
  S = mean_of_medians(factor_name='Species') + mean_centre()
  S = model_train(S,D)
  S = model_predict(S,D)
  expect_equal(S[1]$transformed$data,A$transformed$data)
  expect_equal(dim(predicted(S)),dim(D))
})
