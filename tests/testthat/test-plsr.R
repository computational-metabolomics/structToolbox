# test PLSR
test_that('PLSR',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # check the first scores value
  expect_equal(M[2]$scores$data[1,1],-2.692173222367)
})

test_that('plsr_prediction_plot chart',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)

  # plot
  C=plsr_prediction_plot()
  gg=chart_plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})

test_that('plsr_residual_hist chart',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)

  # plot
  C=plsr_residual_hist()
  gg=chart_plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})

test_that('plsr_residual_hist chart',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)

  # plot
  C=plsr_qq_plot()
  gg=chart_plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})


test_that('plsr_cook_dist',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)

  # plot
  C=plsr_cook_dist()
  gg=chart_plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})
