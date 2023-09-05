# permute sample order
test_that('permute sample order model_seq',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permute_sample_order(number_of_permutations=5)*(mean_centre()+PLSDA(number_components=1,factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  expect_equal(I$metric$mean,expected=0.04,tolerance=0.005)
})

# permute sample order
test_that('permute sample order iterator',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permute_sample_order(number_of_permutations=5)*kfold_xval(folds=5,factor_name='Species')*(mean_centre()+PLSDA(number_components=1,factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  expect_equal(I$metric$mean,expected=0.048,tolerance=0.0005)
})
