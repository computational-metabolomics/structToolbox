# fisher_exact
test_that('ttest',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[D$sample_meta$Species=='versicolor',]=NA
  pred=as.data.frame(is.na(D$data))
  pred=lapply(pred,factor,levels=c(TRUE,FALSE))
  pred=as.data.frame(pred)
  # method
  M = fisher_exact(factor_name='Species',factor_pred=pred)
  # apply
  M = model_apply(M,D)
  expect_true(all(M$significant[,1]))
})
