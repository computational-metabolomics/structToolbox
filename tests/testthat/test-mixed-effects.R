# test mixed effect class
test_that('mixed effects',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Species=D$sample_meta$Species # fixed effect
  D$sample_meta$Rnd=rnorm(150) # random effect
  # method
  ME=mixed_effect(formula=y~Species+Error(Rnd/Species))
  ME=model_apply(ME,D)
  # expect all true
  expect_true(all(ME$significant[,1]))

})

# test HSDEM class
test_that('hsdem',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Species=D$sample_meta$Species # fixed effect
  D$sample_meta$Rnd=rnorm(150) # random effect
  # method
  ME=HSDEM(formula=y~Species+Error(Rnd/Species))
  ME=model_apply(ME,D)
  # expect all true
  expect_true(all(ME$significant[,1]))

})
