# filter smeta
test_that('filter smeta include',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = filter_smeta(mode='include',levels='versicolor',factor_name='Species')
  # apply
  M = method.apply(M,D)
  expect_equal(nrow(M$filtered$data),50)
})

test_that('filter smeta exclude',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = filter_smeta(mode='exclude',levels='versicolor',factor_name='Species')
  # apply
  M = method.apply(M,D)
  expect_equal(nrow(M$filtered$data),100)
})
