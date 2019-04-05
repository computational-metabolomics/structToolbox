# test dataset charts
test_that('feature_boxplot creates ggplot object',{
  # dataset
  D=iris_dataset()
  # chart
  C = feature_boxplot(feature_to_plot=1,factor_name='Species')
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})


test_that('mv_histogram creates ggplot object',{
  # dataset
  D=iris_dataset()
  # add some missing values
  r=sample(nrow(D$data),size=300,replace=TRUE)
  c=sample(ncol(D$data),size=300,replace=TRUE)
  for (k in 1:300) {
    D$data[r[k],c[k]]=NA
  }
  # chart
  C = mv_histogram(by_sample=TRUE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = mv_histogram(by_sample=FALSE)
  # plot
  gg=chart.plot(C,D)
  #ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})


test_that('mv_boxplot creates ggplot object',{
  # dataset
  D=iris_dataset()
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
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = mv_boxplot(by_sample=FALSE,factor_name='Species',show_counts=TRUE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('dataset.dist creates ggplot object',{
  # dataset
  D=iris_dataset()

  # chart
  C = dataset.dist(factor_name='Species',per_class=TRUE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = dataset.dist(factor_name='Species',per_class=FALSE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})

test_that('dataset.boxplot creates ggplot object',{
  # dataset
  D=iris_dataset()
  colnames(D$data)=rownames(D$variable_meta)[1:4]
    # chart
  C = dataset.boxplot(factor_name='Species',per_class=FALSE,by_sample=FALSE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = dataset.boxplot(factor_name='Species',per_class=FALSE,by_sample=TRUE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = dataset.boxplot(factor_name='Species',per_class=TRUE,by_sample=FALSE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = dataset.boxplot(factor_name='Species',per_class=TRUE,by_sample=TRUE)
  # plot
  gg=chart.plot(C,D)

  ggplot_build(gg)
  expect_true(is(gg,'ggplot'))
})



test_that('compare_dist creates ggplot object',{
  # dataset
  D=iris_dataset()
  # chart
  C = compare_dist(factor_name='Species')
  # plot
  gg=chart.plot(C,D,D)
  expect_true(is(gg,'gtable'))
}
)

test_that('dataset.heatmap creates ggplot object',{
  # dataset
  D=iris_dataset()
  # chart
  C = dataset.heatmap()
  # plot
  gg=chart.plot(C,D)
  expect_true(is(gg,'ggplot'))
}
)
