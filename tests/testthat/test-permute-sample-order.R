# permute sample order
test_that('permute sample order model.seq',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = permute_sample_order(number_of_permutations=5)*(mean_centre()+PLSDA(number_components=1))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  expect_equal(I$metric$mean,expected=0.335,tolerance=0.05)
})

# permute sample order
test_that('permute sample order iterator',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = permute_sample_order(number_of_permutations=5)*kfold_xval(folds=5)*(mean_centre()+PLSDA(number_components=1))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  expect_equal(I$metric$mean,expected=0.339,tolerance=0.05)
})
