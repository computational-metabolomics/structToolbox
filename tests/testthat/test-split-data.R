# split data class
test_that('split data',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = split_data(p=0.75)
  # apply
  M = model_apply(M,D)
  expect_equal(nrow(M$testing$data),38)
  expect_equal(nrow(M$training$data),112)
})

test_that('equal split',{
  set.seed('57475')
  # DatasetExperiment with unequal groups (10 setosa, 50 versicolor, 50 virginica)
  D=iris_DatasetExperiment()
  D=D[c(1:10,51:150),]
  # method
  M = equal_split(p_train=0.5,factor_name='Species')
  # apply
  M = model_apply(M,D)
  # 5 from each group in the training set
  expect_equal(as.vector(table(M$training$sample_meta$Species)),c(5,5,5))
  expect_equal(nrow(M$testing$data),110-15)
  # over repeats, samples beyond the first 10 of the larger groups are chosen
  chosen=unlist(lapply(1:20,function(i) {
    rownames(model_apply(M,D)$training$sample_meta)
  }))
  expect_true(any(chosen %in% rownames(D$data)[c(21:60,71:110)]))
})

test_that('split data with no training samples',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # training fraction rounds to 0 samples, so all samples are for testing
  M = model_apply(split_data(p_train=0.001),D)
  expect_equal(nrow(M$training$data),0)
  expect_equal(nrow(M$testing$data),150)
  # same for every group of a stratified split
  M = model_apply(stratified_split(p_train=0.01,factor_name='Species'),D)
  expect_equal(nrow(M$training$data),0)
  expect_equal(nrow(M$testing$data),150)
  # training and testing sets do not overlap and cover all samples
  M = model_apply(stratified_split(p_train=0.5,factor_name='Species'),D)
  expect_equal(sort(c(rownames(M$training$data),rownames(M$testing$data))),
    sort(rownames(D$data)))
})

