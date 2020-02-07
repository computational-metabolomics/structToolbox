# proportion na filter
test_that('prop_na',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[D$sample_meta$Species=='versicolor',]=NA
  # method
  M = prop_na(factor_name='Species')
  # apply
  M=model_apply(M,D)
  expect_true(all(M$significant[,1]))
  expect_true(all(M$na_count$versicolor==50))
})
