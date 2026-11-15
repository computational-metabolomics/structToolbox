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
