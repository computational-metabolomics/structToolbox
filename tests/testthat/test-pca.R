# test PCA
test_that('PCA gives expected sum of sqaures for Iris data',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=PCA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # check the sum of squares
  expect_equal(M$ssx,9539.29)

  # using 1 component
  M$number_components=1
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # check the sum of squares
  expect_equal(M$ssx,9539.29)
})

test_that('PCA scores chart returns ggplot object',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=PCA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # chart
  C=pca_scores_plot(groups=D$sample_meta$Species)
  # plot
  gg=chart.plot(C,M)
  plot(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA biplot chart returns ggplot object',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # chart
  C=pca_biplot_plot(groups=D$sample_meta$Species,style='arrows')
  # plot
  gg=chart.plot(C,M[2])
  plot(gg)
  expect_true(is(gg,'ggplot'))
  # different style
  C=pca_biplot_plot(groups=D$sample_meta$Species,style='points')
  # plot
  gg=chart.plot(C,M[2])
  plot(gg)
  expect_true(is(gg,'ggplot'))
  # variable labels
  C=pca_biplot_plot(groups=D$sample_meta$Species,style='points',label_features = TRUE)
  # plot
  gg=chart.plot(C,M[2])
  plot(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA correlation chart returns ggplot object',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # chart
  C=pca_correlation_plot()
  # plot
  gg=chart.plot(C,M[2])
  plot(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA scree chart returns ggplot object',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # chart
  C=PCA.scree()
  # plot
  gg=chart.plot(C,M[2])
  plot(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('PCA dstat chart returns ggplot object',{
  # dataset
  D=iris_dataset()
  # PCA model
  M=mean_centre()+PCA()
  # train the model
  M=model.train(M,D)
  # apply the model
  M=model.predict(M,D)
  # chart
  C=PCA.dstat()
  # plot
  gg=chart.plot(C,M[2])
  plot(gg)
  expect_true(is(gg,'ggplot'))
})
