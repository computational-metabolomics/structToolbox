# test permutation test class
test_that('permutation test',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=20,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  B=calculate(B,Yhat=output_value(I,'results.unpermuted')$predicted,
    Y=output_value(I,'results.unpermuted')$actual)
  expect_equal(value(B),expected=0.105,tolerance=0.0005)
})

# permutation test box plot
test_that('permutation test boxplot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10,factor_name='Species')*
    kfold_xval(folds=3,factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test_plot(style='boxplot')
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

# permutation test hist plot
test_that('permutation test hist plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10,factor_name='Species')*
    kfold_xval(folds=3,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test_plot(style='histogram')
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

# permutation test scatter plot
test_that('permutation scatter plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10,factor_name='Species')*
    kfold_xval(folds=3,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test_plot(style='scatter')
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

# permutation test violin plot
test_that('permutation violin plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permutation_test(number_of_permutations=10,factor_name='Species')*
    kfold_xval(folds=3,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C=permutation_test_plot(style='violin')
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

