# knn impute
test_that('knn impute',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$data=as.data.frame(apply(D$data,2,function(x){
    n=seq(1,150,by=4)
    s=sample(1:length(n),length(n),replace=TRUE)
    n=n[-s]
    x[n]=NA
    return(x)
  }))
  # method
  M = knn_impute(neighbours=5)
  # apply
  M = method.apply(M,D)
  expect_equal(M$imputed$data[5,1],4.675,tolerance=0.001) # an imputed value
})
