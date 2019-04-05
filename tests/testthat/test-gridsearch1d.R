# test grid search
test_that('grid_search iterator',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
      factor_name='Species',
      search_values=1:4,
      model_index=2,
      max_min='min')*
    kfold_xval(folds=5)*
    (mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$value,0.3)
})

# test grid search
test_that('grid_search wf',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
    factor_name='Species',
    search_values=1:4,
    model_index=2,
    max_min='min')*
    (mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$value[1],0.320)
})

# test grid search
test_that('grid_search chart',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
    factor_name='Species',
    search_values=1:4,
    model_index=2,
    max_min='min')*
    kfold_xval(folds=5)*
    (mean_centre()+PLSDA())
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  C=gs_line()
  gg=chart.plot(C,I)
  expect_true(is(gg,'ggplot'))
})
