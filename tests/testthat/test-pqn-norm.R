# pqn_norm
test_that('pqn norm',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = pqn_norm(qc_label='versicolor',factor_name='Species')
  # apply
  M = method.apply(M,D)
  expect_equal(M$coeff[1,1],0.59,tolerance=0.005)
})

test_that('pqn norm hist',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = pqn_norm(qc_label='versicolor',factor_name='Species')
  # apply
  M = method.apply(M,D)
  #chart
  C = pqn_norm.hist()
  gg=chart.plot(C,M)
  expect_true(is(gg,'ggplot'))
})

