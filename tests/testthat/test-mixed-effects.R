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

# test HSDEM with more than one factor
test_that('hsdem multiple factors',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  D$sample_meta$Rnd=factor(rep(1:25,6)) # random effect
  D$sample_meta$fake=factor(rep(c('a','b'),75)) # second fixed effect
  # a constant feature, so its model fails
  D$data[,4]=1
  # method
  ME=HSDEM(formula=y~Species*fake+Error(Rnd/Species),mtc='none')
  ME=model_apply(ME,D)
  # contrasts for every term are kept
  expect_equal(ncol(ME$p_value),3+1+15)
  expect_equal(colnames(ME$p_value)[1:4],
    c('Species: setosa - versicolor','Species: setosa - virginica',
      'Species: versicolor - virginica','fake: a - b'))
  # p-values match emmeans for the first feature
  temp=D$sample_meta
  temp$y=scale(D$data[,1])
  O=options(contrasts=c("contr.sum","contr.poly"))
  LM=nlme::lme(y~Species*fake,random=~1|Rnd,method='ML',data=temp)
  ref=unlist(lapply(list('Species','fake',c('Species','fake')),function(t)
    suppressMessages(as.data.frame(pairs(emmeans::emmeans(LM,t))))$p.value))
  options(O)
  expect_equal(unname(unlist(ME$p_value[1,])),ref)
  # failed feature is all NA
  expect_true(all(is.na(ME$p_value[4,])))
})
