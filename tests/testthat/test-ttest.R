# ttest
test_that('ttest',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # method
  M = filter_smeta(mode='exclude',levels='versicolor',factor_name='Species')+ # need two groups for ttest
    ttest(factor_names='Species')
  # apply
  M = model_apply(M,D)
  expect_equal(M[2]$t_statistic[1,1],-15.386,tolerance=0.0005)
})

test_that('paired-ttest',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  
  # reduce to two classes
  M=filter_smeta(mode='exclude',levels='setosa',factor_name='Species')
  M=model_apply(M,D)
  D=predicted(M)
  
  # set up paired sampling
  D$sample_meta$pair_id=c(1:50,1:50)
  
  # remove one row so we can check it works for unequal pairs
  D=D[1:99,]
  
  # set one value to NA so we can check the corresponding sample is excluded for that pair
  D$data[20,1]=NA

  # reorder to check we're not order dependent
  D2=D[sample(1:99,99),]
  
  # method
  M = ttest(factor_names='Species',paired=TRUE,paired_factor="pair_id")
  # apply
  M = model_apply(M,D)
  M2= model_apply(M,D2)
  
  # 50 pairs - 1 without pair - 1 with an NA - 1 for ttest
  expect_equal(M$dof[1],47) 
  expect_equal(M$t_statistic[1,1],-5.195,tolerance=0.0005)
  
  # 50 pairs - 1 without pair - 1 for ttest
  expect_equal(M$dof[2],48) 
  expect_equal(M$t_statistic[2,1],-3.014,tolerance=0.0005)
  
  # check order dependence
  expect_equal(M$t_statistic[1,1],M2$t_statistic[1,1],tolerance=0.00005)
  
  # reduce to less than three pairs to check we get NA
  D=D[c(1:3,51:53),]
  D$data[2,2]=NA 
  
  # should get NA for second feature as not enough pairs
  M3=model_apply(M,D)
  expect_true(is.na(M3$t_statistic[2,1]))
})

# ttest
test_that('wilcox',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  
  # need two groups
  M = filter_smeta(mode='exclude',levels='versicolor',factor_name='Species')
  M=model_apply(M,D) # need two groups
  D = predicted(M)
  
  # apply
  M = wilcox_test(factor_names='Species')
  M = model_apply(M,D)
  
  expect_equal(M$statistic[1,1],38.5,tolerance=0.1)
})

test_that('paired-wilcox',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  
  # reduce to two classes
  M=filter_smeta(mode='exclude',levels='setosa',factor_name='Species')
  M=model_apply(M,D)
  D=predicted(M)
  
  # set up paired sampling
  D$sample_meta$pair_id=c(1:50,1:50)
  
  # remove one row so we can check it works for unequal pairs
  D=D[1:99,]
  
  # set one value to NA so we can check the corresponding sample is excluded for that pair
  D$data[20,1]=NA
  
  # reorder to check we're not order dependent
  D2=D[sample(1:99,99),]
  
  # method
  M = wilcox_test(factor_names='Species',paired=TRUE,paired_factor="pair_id")
  # apply
  M = model_apply(M,D)
  M2= model_apply(M,D2)
  
  # 50 pairs - 1 without pair - 1 with an NA - 1 for ttest
  expect_equal(M$statistic[1,1],155.5,tolerance=0.0005)
  
  # 50 pairs - 1 without pair - 1 for ttest
  expect_equal(M$statistic[2,1],238,tolerance=0.0005)
  
  # check order dependence
  expect_equal(M$statistic[1,1],M2$statistic[1,1],tolerance=0.00005)
  
  # reduce to less than three pairs to check we get NA
  D$data[53:99,2]=NA 
  
  # should get NA for second feature as not enough pairs
  M3=model_apply(M,D)
  expect_true(is.na(M3$statistic[2,1]))
})