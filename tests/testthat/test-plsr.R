# test PLSR
test_that('PLSR',{
  # dataset
  D=iris_dataset()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # check the first scores value
  expect_equal(M[2]$scores[1,1],-2.692173222367)
})

test_that('plsr_prediction_plot chart',{
  # dataset
  D=iris_dataset()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)

  # plot
  C=plsr_prediction_plot()
  gg=chart.plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})

test_that('plsr_residual_hist chart',{
  # dataset
  D=iris_dataset()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)

  # plot
  C=plsr_residual_hist()
  gg=chart.plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})

test_that('plsr_residual_hist chart',{
  # dataset
  D=iris_dataset()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)

  # plot
  C=plsr_qq_plot()
  gg=chart.plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})


test_that('plsr_cook_dist',{
  # dataset
  D=iris_dataset()
  D$sample_meta$Species=as.numeric(D$sample_meta$Species)
  # PLSR model
  M=mean_centre(mode='both')+PLSR(factor_name='Species')
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)

  # plot
  C=plsr_cook_dist()
  gg=chart.plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})
