# test PCA
test_that('PCA gives expected sum of sqaures for Iris data',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=PCA()
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # check the sum of squares
  expect_equal(M$ssx,9539.29)

  # using 1 component
  M$number_components=1
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # check the sum of squares
  expect_equal(M$ssx,9539.29)
})

test_that('PCA scores chart returns ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # chart
  C=pca_scores_plot(factor_name='Species')
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # label all points
  C=pca_scores_plot(factor_name='Species',points_to_label = 'all')
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # label outliers
  C=pca_scores_plot(factor_name='Species',points_to_label = 'outliers')
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))


  # continuous factor
  M[2]$scores$sample_meta$Sample_No=1:length(D$sample_meta$Species)
  C=pca_scores_plot(factor_name='Sample_No')
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # label filter
  C=pca_scores_plot(factor_name='Species',label_filter='virginica',points_to_label='all')
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA biplot chart returns ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # chart
  C=pca_biplot_plot(factor_name='Species',style='arrows')
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
  # different style
  C=pca_biplot_plot(factor_name='Species',style='points')
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
  # variable labels
  C=pca_biplot_plot(factor_name='Species',style='points',label_features = TRUE)
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA correlation chart returns ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # chart
  C=pca_correlation_plot()
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA scree chart returns ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # chart
  C=pca_scree()
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA dstat chart returns ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # chart
  C=PCA_dstat()
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA loadings chart returns ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model_train(M,D)
  # apply the model
  M=model_predict(M,D)
  # chart
  C=pca_loadings_plot()
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # only one component
  C=pca_loadings_plot(components=1)
  # plot
  gg=chart_plot(C,M[2])

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

})
