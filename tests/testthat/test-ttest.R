# ttest
test_that('ttest',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  M = filter_smeta(mode='exclude',levels='versicolor',factor_name='Species')+ # need two groups for ttest
    ttest(factor_names='Species')
  # apply
  M = method.apply(M,D)
  expect_equal(M[2]$t_statistic[1],-15.386,tolerance=0.0005)
})

