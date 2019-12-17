# test permutation test class
test_that('permutation test',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=20)*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  B=calculate(B,Yhat=output_value(I,'results.unpermuted')$predicted,
    Y=output_value(I,'results.unpermuted')$actual)
  expect_equal(value(B),expected=0.50775,tolerance=0.004)
})

# permutation test box plot
test_that('permutation test boxplot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10)*kfold_xval(folds=3,factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test.boxplot()
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

# permutation test hist plot
test_that('permutation test hist plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10)*kfold_xval(folds=3,factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test_hist()
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

# permutation test scatter plot
test_that('permutation scatter hist plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10)*kfold_xval(folds=3,factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test.scatter()
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

# permutation test violin plot
test_that('permutation violin hist plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10)*kfold_xval(folds=3,factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test.violin()
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

