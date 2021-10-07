# fold_change
test_that('fold_change',{
  set.seed('57475')
  
  # data
  D = iris_DatasetExperiment()
  
  # two groups
  F = filter_smeta(mode='exclude',levels='setosa',factor_name='Species')
  F = model_apply(F,D)
  
  D = predicted(F)
  
  # add column for paired data
  D$sample_meta$sample_id=c(1:50,1:50)
  D$data[1:50,2] = NA
  D$data[1:25,3] = NA
  
  # unpaired
  FF = fold_change(factor_name='Species',method="geometric")
  FF = model_apply(FF,D)
  m=exp(mean(log(D$data[D$sample_meta$Species=='virginica',1]))) / exp(mean(log((D$data[D$sample_meta$Species=='versicolor',1]))))
  expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
  expect_true(is.na(FF$fold_change[2,1]))
  m=exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='virginica',3])))) / exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='versicolor',3]))))
  expect_equal(FF$fold_change[3,1],m,tolerance=0.00001)
  
  FF$method = 'median'
  FF = model_apply(FF,D)
  m = median(D$data[D$sample_meta$Species=='virginica',1]) / median(D$data[D$sample_meta$Species=='versicolor',1])
  expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
  expect_true(is.na(FF$fold_change[2,1]))
  m = median(D$data[D$sample_meta$Species=='virginica',3],na.rm = TRUE) / median(D$data[D$sample_meta$Species=='versicolor',3],na.rm = TRUE)
  expect_equal(FF$fold_change[3,1],m,tolerance=0.00001)
  
  FF$method = 'mean'
  FF = model_apply(FF,D)
  m = mean(D$data[D$sample_meta$Species=='virginica',1]) / mean(D$data[D$sample_meta$Species=='versicolor',1])
  expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
  expect_true(is.na(FF$fold_change[2,1]))
  m = mean(D$data[D$sample_meta$Species=='virginica',3],na.rm=TRUE) / mean(D$data[D$sample_meta$Species=='versicolor',3],na.rm=TRUE)
  expect_equal(FF$fold_change[3,1],m,tolerance=0.00001)
  
  # paired
  FF = fold_change(factor_name='Species',method="geometric",paired=TRUE,sample_name = 'sample_id')
  FF = model_apply(FF,D)
  m=exp(mean(log(D$data[D$sample_meta$Species=='virginica',1])-log(D$data[D$sample_meta$Species=='versicolor',1])))
  expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
  expect_true(is.na(FF$fold_change[2,1]))
  
  FF$method = 'median'
  FF = model_apply(FF,D)
  m = median(D$data[D$sample_meta$Species=='virginica',1] / D$data[D$sample_meta$Species=='versicolor',1])
  expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
  expect_true(is.na(FF$fold_change[2,1]))
  
  FF$method = 'mean'
  FF = model_apply(FF,D)
  m = mean(D$data[D$sample_meta$Species=='virginica',1] / D$data[D$sample_meta$Species=='versicolor',1])
  expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
  expect_true(is.na(FF$fold_change[2,1]))


})
