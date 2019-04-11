# autoscale
test_that('autoscale',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = autoscale()
  # apply
  M = model.train(M,D)
  # predict
  M = model.predict(M,D)
  expect_equal(M$autoscaled$data[1,1],-0.897,tolerance=0.001)
})
