# test PLSDA
test_that('PLSDA',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PLSDA(factor_name='Species')
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # check the first scores value
  expect_equal(M[2]$scores$data[1,1],2.69582435)
})

test_that('plsda scores chart',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PLSDA(factor_name='Species')
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)

  # scores plot
  C=plsda_scores_plot(factor_name='Species')
  gg=chart_plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # scores plot
  C=plsda_scores_plot(factor_name='Species',points_to_label='outliers')
  gg=chart_plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # scores plot
  C=plsda_scores_plot(factor_name='Species',points_to_label='all')
  gg=chart_plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})
