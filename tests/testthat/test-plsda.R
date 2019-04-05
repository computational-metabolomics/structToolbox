# test PLSDA
test_that('PLSDA',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=mean_centre()+PLSDA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # check the first scores value
  expect_equal(M[2]$scores[1,1],2.69582435)
})

test_that('plsda scores chart',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=mean_centre()+PLSDA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)

  # scores plot
  C=plsda_scores_plot(factor_name='Species',groups=D$sample_meta$Species)
  gg=chart.plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # scores plot
  C=plsda_scores_plot(factor_name='Species',groups=D$sample_meta$Species,points_to_label='outliers')
  gg=chart.plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # scores plot
  C=plsda_scores_plot(factor_name='Species',groups=D$sample_meta$Species,points_to_label='all')
  gg=chart.plot(C,M[2])
  g=ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})
