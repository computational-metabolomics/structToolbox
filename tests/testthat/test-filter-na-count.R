# filter_na_count
test_that('filter_na_count',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[1:48,1]=NA # 2 measured values for setosa
  # method
  M = filter_na_count(threshold=3,factor_name='Species')
  M = model_apply(M,D)
  # only the first feature is removed
  expect_equal(M$flags$flags,c(TRUE,FALSE,FALSE,FALSE))
  expect_equal(ncol(predicted(M)),3)
})

test_that('filter_na_count empty groups',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[1:48,1]=NA # 2 measured values for setosa
  # unused level
  D2=D
  D2$sample_meta$Species=factor(D2$sample_meta$Species,
    levels=c(levels(D2$sample_meta$Species),'unused'))
  # missing combination of two factors (no setosa in group 'b')
  D3=D
  D3$sample_meta$grp=factor(c(rep('a',50),rep(c('a','b'),50)))
  # default: empty groups are included, so every feature is removed
  M = model_apply(filter_na_count(threshold=3,factor_name='Species'),D2)
  expect_true(all(M$flags$flags))
  M = model_apply(filter_na_count(threshold=3,factor_name=c('Species','grp')),D3)
  expect_true(all(M$flags$flags))
  # drop_empty: empty groups are ignored
  M = model_apply(filter_na_count(threshold=3,factor_name='Species',drop_empty=TRUE),D2)
  expect_equal(M$flags$flags,c(TRUE,FALSE,FALSE,FALSE))
  M = model_apply(filter_na_count(threshold=3,factor_name=c('Species','grp'),drop_empty=TRUE),D3)
  expect_equal(M$flags$flags,c(TRUE,FALSE,FALSE,FALSE))
})
