# filter d-ratio
test_that('d-ratio filter works as expected using sd ratio',{
    set.seed('57475')
    # DatasetExperiment
    DE = MTBLS79_DatasetExperiment(filtered=TRUE)
    M = dratio_filter(
        threshold=20,
        qc_label = 'QC',
        factor_name='Class',
        method = 'ratio',
        dispersion = 'sd'
    )
    M = model_apply(M,DE)
    
    # manually compute d-ratio
    qc_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class=='QC',],sd,na.rm=TRUE)
    )
    sa_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class!='QC',],sd,na.rm=TRUE)
    )
    dr = (qc_mad/(sa_mad))*100
    
    # check manual vs fcn
    expect_true(all(dr==M$d_ratio$d_ratio))
    
    # check values havent changed
    expect_equal(dr[[1]],23.05762,tolerance = 5e-6)
    expect_equal(dr[[100]],64.49242,tolerance = 5e-6)
    
    # just number of filtered columns
    expect_true(ncol(predicted(M))==725)
    expect_true(ncol(DE)-ncol(predicted(M))==854)
    expect_true(ncol(DE)-ncol(predicted(M))==sum(dr>20))
})

test_that('d-ratio filter works as expected using mad ratio',{
    set.seed('57475')
    # DatasetExperiment
    DE = MTBLS79_DatasetExperiment(filtered=TRUE)
    M = dratio_filter(
        threshold=20,
        qc_label = 'QC',
        factor_name='Class',
        method = 'ratio',
        dispersion = 'mad'
    )
    M = model_apply(M,DE)
    
    # manually compute d-ratio
    qc_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class=='QC',],mad,na.rm=TRUE)
    )
    sa_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class!='QC',],mad,na.rm=TRUE)
    )
    dr = (qc_mad/(sa_mad))*100
    
    # check manual vs fcn
    expect_true(all(dr==M$d_ratio$d_ratio))
    
    # check values havent changed
    expect_equal(dr[[1]],14.14758,tolerance = 5e-6)
    expect_equal(dr[[100]],48.23871,tolerance = 5e-6)
    
    # just number of filtered columns
    expect_true(ncol(predicted(M))==834)
    expect_true(ncol(DE)-ncol(predicted(M))==745)
    expect_true(ncol(DE)-ncol(predicted(M))==sum(dr>20))
})

test_that('d-ratio filter works as expected using sd euclidean',{
    set.seed('57475')
    # DatasetExperiment
    DE = MTBLS79_DatasetExperiment(filtered=TRUE)
    M = dratio_filter(
        threshold=20,
        qc_label = 'QC',
        factor_name='Class',
        method = 'euclidean',
        dispersion = 'sd'
    )
    M = model_apply(M,DE)
    
    # manually compute d-ratio
    qc_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class=='QC',],sd,na.rm=TRUE)
    )
    sa_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class!='QC',],sd,na.rm=TRUE)
    )
    dr = qc_mad/sqrt((qc_mad^2) + (sa_mad^2))*100
    
    # check manual vs fcn
    expect_true(all(dr==M$d_ratio$d_ratio))
    
    # check values havent changed
    expect_equal(dr[[1]],22.46809,tolerance = 5e-6)
    expect_equal(dr[[100]],54.19862,tolerance = 5e-6)
    
    # just number of filtered columns
    expect_true(ncol(predicted(M))==747)
    expect_true(ncol(DE)-ncol(predicted(M))==832)
    expect_true(ncol(DE)-ncol(predicted(M))==sum(dr>20))
})

test_that('d-ratio filter works as expected using mad euclidean',{
    set.seed('57475')
    # DatasetExperiment
    DE = MTBLS79_DatasetExperiment(filtered=TRUE)
    M = dratio_filter(
        threshold=20,
        qc_label = 'QC',
        factor_name='Class',
        method = 'euclidean',
        dispersion = 'mad'
    )
    M = model_apply(M,DE)
    
    # manually compute d-ratio
    qc_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class=='QC',],mad,na.rm=TRUE)
    )
    sa_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class!='QC',],mad,na.rm=TRUE)
    )
    dr = qc_mad/sqrt((qc_mad^2) + (sa_mad^2))*100
    
    # check manual vs fcn
    expect_true(all(dr==M$d_ratio$d_ratio))
    
    # check values havent changed
    expect_equal(dr[[1]],14.00808,tolerance = 5e-6)
    expect_equal(dr[[100]],43.44776,tolerance = 5e-6)
    
    # just number of filtered columns
    expect_true(ncol(predicted(M))==850)
    expect_true(ncol(DE)-ncol(predicted(M))==729)
    expect_true(ncol(DE)-ncol(predicted(M))==sum(dr>20))
})
