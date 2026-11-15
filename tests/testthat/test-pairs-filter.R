# pairs filter
test_that('pairs filter',{
  set.seed('57475')
  # id 1: A,B (ok); 2: A,A; 3: B,B; 4: A,A,A,B; 5: A only
  smeta=data.frame(
    id=c(1,1,2,2,3,3,4,4,4,4,5),
    group=factor(c('A','B','A','A','B','B','A','A','A','B','A')))
  D=DatasetExperiment(
    data=data.frame(x=rnorm(11),y=rnorm(11)),
    sample_meta=smeta,
    variable_meta=data.frame(name=c('x','y'),row.names=c('x','y')))
  # method
  M = pairs_filter(factor_name='group',sample_id='id')
  M = model_apply(M,D)
  # only id 1 has all levels represented equally
  expect_equal(M$flags$flags,c(0,0,1,1,1,1,1,1,1,1,1))
  expect_equal(predicted(M)$sample_meta$id,c(1,1))
})
