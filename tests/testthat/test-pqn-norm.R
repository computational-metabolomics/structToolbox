# pqn_norm
test_that('pqn norm',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = pqn_norm(qc_label='versicolor',factor_name='Species')
  # apply
  M = model_apply(M,D)
  expect_equal(M$coeff[1,1],0.59,tolerance=0.005)
})

test_that('pqn norm hist',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = pqn_norm(qc_label='versicolor',factor_name='Species')
  # apply
  M = model_apply(M,D)
  #chart
  C = pqn_norm_hist()
  gg=chart_plot(C,M)
  expect_true(is(gg,'ggplot'))
})

