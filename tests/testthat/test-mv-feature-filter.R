# test pmp functions
test_that('pmp mv_feature within_all',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[,1]=NA

  # filter
  FF=mv_feature_filter(qc_label='versicolor',method='within_all',factor_name='Species',threshold = 20)
  FF=model_apply(FF,D)
  expect_equal(ncol(FF$filtered$data),3)
})

test_that('pmp mv_feature within_one',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[,1]=NA

  # filter
  FF=mv_feature_filter(qc_label='versicolor',method='within_one',factor_name='Species')
  FF=model_apply(FF,D)
  expect_equal(ncol(FF$filtered$data),3)
})

test_that('pmp mv_feature across',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[,1]=NA

  # filter
  FF=mv_feature_filter(qc_label='versicolor',method='across',factor_name='Species')
  FF=model_apply(FF,D)
  expect_equal(ncol(FF$filtered$data),3)
})

test_that('pmp mv_feature qc',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data[,1]=NA

  # filter
  FF=mv_feature_filter(qc_label='versicolor',method='QC',factor_name='Species')
  FF=model_apply(FF,D)
  expect_equal(ncol(FF$filtered$data),3)
})

test_that('pmp mv_feature_plot', {
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$data=as.data.frame(apply(D$data,2,function(x){
    s=sample(1:150,150,replace=TRUE)
    x[s]=NA
    return(x)
  }))

  # filter
  FF=mv_feature_filter(qc_label='versicolor',method='across',factor_name='Species')
  FF=model_apply(FF,D)
  C=mv_feature_filter_hist()
  gg=chart_plot(C,FF)
  expect_true(is(gg,'ggplot'))
})
