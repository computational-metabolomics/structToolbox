# nroot
test_that('nroot uses the requested root',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # default is square root
  M = model_apply(nroot_transform(),D)
  expect_equal(M$transformed$data[1,1],sqrt(5.1))
  # non-default root
  M = model_apply(nroot_transform(root=3),D)
  expect_equal(M$root,3)
  expect_equal(M$transformed$data[1,1],5.1^(1/3))
})

test_that('nroot_transform train then predict',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # reference
  A = model_apply(nroot_transform(root=3),D)
  # train then predict on its own
  M = model_train(nroot_transform(root=3),D)
  M = model_predict(M,D)
  expect_equal(M$transformed$data,A$transformed$data)
  # train then predict in a model sequence
  S = nroot_transform(root=3) + mean_centre()
  S = model_train(S,D)
  S = model_predict(S,D)
  expect_equal(S[1]$transformed$data,A$transformed$data)
  expect_equal(dim(predicted(S)),dim(D))
})
