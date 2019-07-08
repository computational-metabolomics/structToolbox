# test confounders lsq
test_that('confounders lsq',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$sample_meta$Rnd=sample(D$sample_meta$Species,150) # random effect
  # method
  ME=confounders_clsq(factor_name='Species',confounding_factors='Rnd',threshold=0.15)
  ME=method.apply(ME,D)
  # expect all true
  expect_true(all(as.matrix(ME$significant)))

})

# confounders_lsq plots
# test confounders lsq
test_that('confounders lsq barchart',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$sample_meta$Rnd=sample(D$sample_meta$Species,150) # random effect
  # method
  ME=confounders_clsq(factor_name='Species',confounding_factors='Rnd',threshold=0.15)
  ME=method.apply(ME,D)
  # chart
  C=confounders_lsq.barchart(feature_to_plot='Petal.Length')
  gg=chart.plot(C,ME)
  # expect all true
  expect_true(is(gg,'ggplot'))

})

# test confounders lsq
test_that('confounders lsq boxplot',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$sample_meta$Rnd=sample(D$sample_meta$Species,150) # random effect
  # method
  ME=confounders_clsq(factor_name='Species',confounding_factors='Rnd',threshold=0.15)
  ME=method.apply(ME,D)
  # chart
  C=confounders_lsq.boxplot()
  gg=chart.plot(C,ME)
  # expect all true
  expect_true(is(gg,'ggplot'))

})


