# constant sum norm
test_that('constant sum norm',{
    set.seed('57475')
    # DatasetExperiment
    D=iris_DatasetExperiment()
    # method
    M = constant_sum_norm()
    # apply
    M = model_apply(M,D)
    expect_equal(sum(M$normalised$data[1,]),1,tolerance=0.00001)
    expect_equal(sum(M$normalised$data[5,]),1,tolerance=0.00001)
    expect_equal(sum(M$normalised$data[25,]),1,tolerance=0.00001)
    expect_equal(predicted(M)$data[1,1],0.5,tolerance=0.0001)
})

test_that('constant sum norm scaling factor',{
    set.seed('57475')
    # DatasetExperiment
    D=iris_DatasetExperiment()
    # method
    M = constant_sum_norm(scaling_factor = 100)
    # apply
    M = model_apply(M,D)
    expect_equal(sum(M$normalised$data[1,]),100,tolerance=0.00001)
    expect_equal(sum(M$normalised$data[5,]),100,tolerance=0.00001)
    expect_equal(sum(M$normalised$data[25,]),100,tolerance=0.00001)
    expect_equal(predicted(M)$data[1,1],50,tolerance=0.0001)
})
