# filter by name
test_that('filter by name, exclude samples',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = filter_by_name(mode='exclude',dimension='sample',names=c('1','2','3'))
  # apply
  M=model.apply(M,D)
  expect_equal(nrow(M$filtered$data),147)
})

test_that('filter by name, include samples',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = filter_by_name(mode='include',dimension='sample',names=c('1','2','3'))
  # apply
  M=model.apply(M,D)
  expect_equal(nrow(M$filtered$data),3)
})

test_that('filter by name, exclude variables',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = filter_by_name(mode='exclude',dimension='variable',names=c('Petal.Width','Petal.Length'))
  # apply
  M=model.apply(M,D)
  expect_equal(ncol(M$filtered$data),2)
})

test_that('filter by name, include variables',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = filter_by_name(mode='include',dimension='variable',names=c('Petal.Width','Petal.Length'))
  # apply
  M=model.apply(M,D)
  expect_equal(ncol(M$filtered$data),2)
})
