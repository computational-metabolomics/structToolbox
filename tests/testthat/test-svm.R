# SVM
test_that('SVM class weights',{
  # DatasetExperiment with unequal groups
  D=iris_DatasetExperiment()
  D=D[c(1:20,51:150),]
  y=D$sample_meta$Species
  ref=function(cw) {
    e1071::svm(x=D$data,y=y,kernel='linear',gamma=1,degree=3,coef0=0,
      cost=1,class.weights=cw,scale=FALSE)$coefs
  }
  # named vector of weights is used as given
  cw=c(setosa=1,versicolor=2,virginica=3)
  M = model_train(SVM(factor_name='Species',class_weights=cw),D)
  expect_equal(M$coefs,ref(cw))
  # a single weight is used for every class
  M = model_train(SVM(factor_name='Species',class_weights=2),D)
  expect_equal(M$coefs,ref(c(setosa=2,versicolor=2,virginica=2)))
  # inverse weights
  M = model_train(SVM(factor_name='Species',class_weights='inverse'),D)
  expect_equal(M$coefs,ref('inverse'))
})
