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

test_that('plsda predict on new data',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  tr=sample(150,100)
  # method
  M = mean_centre() + PLSDA(factor_name='Species',number_components=2,pred_method='max_yhat')
  M = model_train(M,D[tr,])
  M = model_predict(M,D[-tr,])
  # yhat, scores and probabilities are for the new samples
  expect_equal(nrow(M[2]$yhat),50)
  expect_equal(nrow(M[2]$scores$data),50)
  expect_equal(rownames(M[2]$scores$data),rownames(D$data)[-tr])
  expect_equal(nrow(M[2]$probability),50)
  # labels are the class with the largest yhat (pred_method='max_yhat')
  expect_equal(as.character(M[2]$predicted_labels[,1]),
    colnames(M[2]$yhat)[apply(M[2]$yhat,1,which.max)])
})
