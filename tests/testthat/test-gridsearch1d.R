# test grid search
test_that('grid_search iterator',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
      factor_name='Species',
      search_values=as.numeric(1:4),
      model_index=2,
      max_min='min')*
    kfold_xval(folds=5,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_error()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$value,0.06,tolerance=0.0005)
})

# test grid search
test_that('grid_search wf',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
    factor_name='Species',
    search_values=as.numeric(1:4),
    model_index=2,
    max_min='max')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$value[1],0.947,tolerance=0.005)
})

# test grid search
test_that('grid_search chart',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
    factor_name='Species',
    search_values=as.numeric(1:4),
    model_index=2,
    max_min='min')*
    kfold_xval(folds=5,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_error()
  # run
  I=run(I,D,B)
  # calculate metric
  C=gs_line()
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})

test_that('grid_search wf with search values not 1:k',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  make_gs=function(sv) {
    grid_search_1d(param_to_optimise='number_components',
      factor_name='Species',
      search_values=sv,
      model_index=2,
      max_min='max')*
      (mean_centre()+PLSDA(factor_name='Species'))
  }
  # metric
  B=balanced_accuracy()
  # run
  I1=run(make_gs(as.numeric(1:4)),D,B)
  I2=run(make_gs(c(2,3)),D,B)
  # metric for each value should not depend on the other values searched
  expect_equal(I2$metric$value,I1$metric$value[2:3])
})

test_that('first_min',{
  # first value when nothing is better
  expect_equal(first_min(c(1,2,3)),1)
  # last value can be chosen
  expect_equal(first_min(c(3,2,1)),3)
  expect_equal(first_min(c(2,1)),2)
  expect_equal(first_min(5),1)
  # a later minimum must be better by the threshold
  expect_equal(first_min(c(1,2,0.99,2)),1)
  expect_equal(first_min(c(1,2,0.5,2)),3)
  # negative values (max_min='max'): a worse later minimum is not chosen
  expect_equal(first_min(-c(0.90,0.80,0.895,0.5)),1)
  expect_equal(first_min(-c(0.50,0.80,0.95,0.5)),3)
})
