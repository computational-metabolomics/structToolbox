# testanova class
test_that('anova 1way',{
  # dataset
  D=iris_dataset()
  # method
  ME=ANOVA(formula=y~Species)
  ME=model.apply(ME,D)
  # expect all true
  expect_true(all(ME$significant[,1]))

})

test_that('anova 2way',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$sample_meta$fake_news=sample(D$sample_meta$Species,150,replace=FALSE)
  # method
  ME=ANOVA(formula=y~Species*fake_news)
  ME=model.apply(ME,D)
  # expect all true
  expect_true(all(ME$significant[,1]))

})

# test HSDEM class
test_that('hsd 1 factor',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  # method
  ME=HSD(formula=y~Species)
  ME=model.apply(ME,D)
  # expect all true
  expect_true(all(ME$significant[,1]))

})

# test HSDEM class
test_that('hsd 2 factors',{
  set.seed('57475')
  # dataset
  D=iris_dataset()
  D$sample_meta$fake_news=sample(D$sample_meta$Species,150,replace=FALSE)
  # method
  ME=HSD(formula=y~Species*fake_news)
  ME=model.apply(ME,D)
  # expect all true
  expect_true(all(ME$significant[,1]))
})
