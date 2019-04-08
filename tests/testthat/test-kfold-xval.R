# test kfold_xval class
test_that('kfold xval venetian',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = kfold_xval(folds=5,method='venetian')*(mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$mean,0.23)
})

test_that('kfold xval blocks',{
  set.seed('57475')
  # iterator
  I = kfold_xval(folds=5,method='blocks')*(mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$mean,0.305)
})

test_that('kfold xval random',{
  # iterator
  set.seed(57475)
  I = kfold_xval(folds=5,method='random')*(mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$mean,0.2)
})

test_that('kfold xval metric plot',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = kfold_xval(folds=5,method='venetian')*(mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C = kfoldxcv_metric()
  gg=chart.plot(C,I)
  expect_true(is(gg,'ggplot'))
})

test_that('kfold xval grid plot',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = kfold_xval(folds=5,method='venetian')*(mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # chart
  C = kfoldxcv_grid()
  gg=chart.plot(C,I)
  expect_true(is(gg[[1]],'ggplot'))
})