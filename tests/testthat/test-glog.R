# glog
test_that('glog',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = glog_transform(qc_label='versicolor')
  # apply
  M = method.apply(M,D)
  expect_equal(M$transformed$data[1,1],-0.326,tolerance=0.001)
})
