# tSNE
test_that('tSNE predicted and init',{
  set.seed('57475')
  # DatasetExperiment (remove duplicate samples)
  D=iris_DatasetExperiment()
  D=D[!duplicated(D$data),]
  n=nrow(D)
  # method
  M = tSNE(max_iter=10)
  M = model_apply(M,D)
  # predicted output is the embedding
  P = predicted(M)
  expect_true(is(P,'DatasetExperiment'))
  expect_equal(dim(P$data),c(n,2))
  # initial coordinates as a data.frame or DatasetExperiment
  init=data.frame(x=rnorm(n),y=rnorm(n),row.names=rownames(D$data))
  M = model_apply(tSNE(max_iter=10,init=init),D)
  expect_equal(dim(predicted(M)$data),c(n,2))
  initD=DatasetExperiment(data=init,sample_meta=D$sample_meta,variable_meta=colnames(init))
  M = model_apply(tSNE(max_iter=10,init=initD),D)
  expect_equal(dim(predicted(M)$data),c(n,2))
})
