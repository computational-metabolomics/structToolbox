# test factor barchart class
test_that('factor_barchart 1 factor',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # chart
  C=DatasetExperiment_factor_boxplot(feature_to_plot='Petal.Length',factor_names='Species')
  gg=chart_plot(C,D)
  expect_true(is(gg,'ggplot'))
})

# test factor barchart class
test_that('factor_barchart 2 factor',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # add random factor
  D$sample_meta$rnd1=sample(D$sample_meta$Species,150)
  # chart
  C=DatasetExperiment_factor_boxplot(feature_to_plot='Petal.Length',factor_names=c('Species','rnd1'))
  gg=chart_plot(C,D)
  expect_true(is(gg,'ggplot'))
})

# test factor barchart class
test_that('factor_barchart 3 factor',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # add random factors
  D$sample_meta$rnd1=sample(D$sample_meta$Species,150)
  D$sample_meta$rnd2=sample(D$sample_meta$Species,150)
  # chart
  C=DatasetExperiment_factor_boxplot(feature_to_plot='Petal.Length',factor_names=c('Species','rnd1','rnd2'))
  gg=chart_plot(C,D)
  expect_true(is(gg,'gtable'))
})
