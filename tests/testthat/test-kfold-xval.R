# test kfold_xval class
test_that('kfold xval venetian',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = kfold_xval(folds=5,method='venetian',factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$mean,0.23,tolerance=0.05)
})

test_that('kfold xval blocks',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = permute_sample_order(number_of_permutations=1)* # needs to be permuted or all groups not in training set
    kfold_xval(folds=5,method='blocks',factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$mean,0.23,tolerance=0.05)
})

test_that('kfold xval random',{
  set.seed(57475)
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = kfold_xval(folds=5,method='random',factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$mean,0.23,tolerance=0.05)
})

test_that('kfold xval metric plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = kfold_xval(folds=5,method='venetian',factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C = kfoldxcv_metric()
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

test_that('kfold xval grid plot',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = kfold_xval(folds=5,method='venetian',factor_name='Species')*(mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C = kfoldxcv_grid(factor_name='Species')
  gg=chart_plot(C,I)
  expect_true(is(gg[[1]],'ggplot'))
})
