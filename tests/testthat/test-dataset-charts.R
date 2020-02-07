# test DatasetExperiment charts
test_that('feature_boxplot creates ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # chart
  C = feature_boxplot(feature_to_plot=1,factor_name='Species')
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})


test_that('mv_histogram creates ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # add some missing values
  r=sample(nrow(D$data),size=300,replace=TRUE)
  c=sample(ncol(D$data),size=300,replace=TRUE)
  for (k in 1:300) {
    D$data[r[k],c[k]]=NA
  }
  # chart
  C = mv_histogram(by_sample=TRUE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = mv_histogram(by_sample=FALSE)
  # plot
  gg=chart_plot(C,D)
  #ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})


test_that('mv_boxplot creates ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  colnames(D$data)=rownames(D$variable_meta)[1:4]
  # add some missing values
  r=sample(nrow(D$data),size=300,replace=TRUE)
  c=sample(ncol(D$data),size=300,replace=TRUE)
  for (k in 1:300) {
    D$data[r[k],c[k]]=NA
  }
  # chart
  C = mv_boxplot(by_sample=TRUE,factor_name='Species',show_counts=TRUE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = mv_boxplot(by_sample=FALSE,factor_name='Species',show_counts=TRUE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('DatasetExperiment.dist creates ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()

  # chart
  C = DatasetExperiment.dist(factor_name='Species',per_class=TRUE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = DatasetExperiment.dist(factor_name='Species',per_class=FALSE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('DatasetExperiment.boxplot creates ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  colnames(D$data)=rownames(D$variable_meta)[1:4]
    # chart
  C = DatasetExperiment.boxplot(factor_name='Species',per_class=FALSE,by_sample=FALSE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = DatasetExperiment.boxplot(factor_name='Species',per_class=FALSE,by_sample=TRUE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = DatasetExperiment.boxplot(factor_name='Species',per_class=TRUE,by_sample=FALSE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = DatasetExperiment.boxplot(factor_name='Species',per_class=TRUE,by_sample=TRUE)
  # plot
  gg=chart_plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})



test_that('compare_dist creates ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # chart
  C = compare_dist(factor_name='Species')
  # plot
  gg=chart_plot(C,D,D)
  expect_true(is(gg,'gtable'))
}
)

test_that('DatasetExperiment.heatmap creates ggplot object',{
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # chart
  C = DatasetExperiment.heatmap()
  # plot
  gg=chart_plot(C,D)
  expect_true(is(gg,'ggplot'))
}
)
