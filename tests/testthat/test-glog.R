# glog
test_that('glog',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = glog_transform(qc_label='setosa',factor_name='Species')
  # apply
  M = model.apply(M,D)
  expect_equal(M$transformed$data[1,1],2.31,tolerance=0.05)
})
