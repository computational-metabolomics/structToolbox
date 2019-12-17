# ttest
test_that('ttest',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = filter_smeta(mode='exclude',levels='versicolor',factor_name='Species')+ # need two groups for ttest
    ttest(factor_names='Species')
  # apply
  M = model_apply(M,D)
  expect_equal(M[2]$t_statistic[1],-15.386,tolerance=0.0005)
})

