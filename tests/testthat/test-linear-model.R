# linear model
test_that('linear model predict',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = linear_model(formula=Sepal.Length~Petal.Length+Species)
  # apply: predictions on the training data are the fitted values
  M = model_apply(M,D)
  expect_equal(unname(M$predicted_values),unname(M$fitted_values))
  # predict new data
  tr = sample(150,100)
  M = model_train(M,D[tr,])
  M = model_predict(M,D[-tr,])
  ref = predict(lm(Sepal.Length~Petal.Length+Species,data=cbind(D$data,D$sample_meta)[tr,]),
    newdata=cbind(D$data,D$sample_meta)[-tr,])
  expect_equal(unname(M$predicted_values),unname(ref))
})
