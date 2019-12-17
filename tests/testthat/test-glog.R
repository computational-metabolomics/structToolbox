# glog
test_that('glog',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = glog_transform(qc_label='setosa',factor_name='Species')
  # apply
  M = model_apply(M,D)
  expect_equal(M$transformed$data[1,1],2.31,tolerance=0.05)
})
