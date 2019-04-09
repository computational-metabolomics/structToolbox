# test factor barchart class
test_that('factor_barchart 1 factor',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # chart
  C=dataset.factor_barchart(feature_to_plot='Petal.Length',factor_names='Species')
  gg=chart.plot(C,D)
  expect_true(is(gg,'ggplot'))
})

# test factor barchart class
test_that('factor_barchart 2 factor',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # add random factor
  D$sample_meta$rnd1=sample(D$sample_meta$Species,150)
  # chart
  C=dataset.factor_barchart(feature_to_plot='Petal.Length',factor_names=c('Species','rnd1'))
  gg=chart.plot(C,D)
  expect_true(is(gg,'ggplot'))
})

# test factor barchart class
test_that('factor_barchart 3 factor',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # add random factors
  D$sample_meta$rnd1=sample(D$sample_meta$Species,150)
  D$sample_meta$rnd2=sample(D$sample_meta$Species,150)
  # chart
  C=dataset.factor_barchart(feature_to_plot='Petal.Length',factor_names=c('Species','rnd1','rnd2'))
  gg=chart.plot(C,D)
  expect_true(is(gg,'gtable'))
})
