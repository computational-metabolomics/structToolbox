# test dataset charts
test_that('feature_boxplot creates ggplot object',{
  # dataset
  D=iris_dataset()
  # chart
  C = feature_boxplot(feature_to_plot=1,factor_name='Species')
  # plot
  gg=chart.plot(C,D)
  plot(gg)
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
  plot(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = mv_histogram(by_sample=FALSE)
  # plot
  gg=chart.plot(C,D)
  plot(gg)
  expect_true(is(gg,'ggplot'))
})


test_that('mv_boxplot creates ggplot object',{
  # dataset
  D=iris_dataset()
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
  plot(gg)
  expect_true(is(gg,'ggplot'))

  # chart
  C = mv_boxplot(by_sample=FALSE,factor_name='Species',show_counts=TRUE)
  # plot
  gg=chart.plot(C,D)
  plot(gg)
  expect_true(is(gg,'ggplot'))
})
