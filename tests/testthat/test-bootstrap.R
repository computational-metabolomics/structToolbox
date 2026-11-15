# bootstrap
test_that('bootstrap collects outputs for each repetition',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = bootstrap(number_of_repetitions=3,collect='vip')*
    (mean_centre()+PLSDA(factor_name='Species',number_components=2))
  # run
  I = run(I,D,balanced_accuracy())
  # one collected output per repetition
  expect_equal(length(I$collected),3)
  expect_equal(dim(I$collected[[1]]),c(4,3))
  # one set of results per repetition
  expect_equal(nrow(I$results),3*150)
  expect_equal(unique(I$results$permutation),1:3)
})
