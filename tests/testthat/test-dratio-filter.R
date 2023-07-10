# filter d-ratio
test_that('d-ratio filter works as expected',{
    set.seed('57475')
    # DatasetExperiment
    DE = MTBLS79_DatasetExperiment(filtered=TRUE)
    M = dratio_filter(
        threshold=20,
        qc_label = 'QC',
        factor_name='Class'
    )
    M = model_apply(M,DE)
    
    # manually compute d-ratio
    qc_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class=='QC',],mad,na.rm=TRUE)
    )
    sa_mad=unlist(
        lapply(DE$data[DE$sample_meta$Class!='QC',],mad,na.rm=TRUE)
    )
    dr = (qc_mad/(qc_mad+sa_mad))*100
    
    # check manual vs fcn
    expect_true(all(dr==M$d_ratio$d_ratio))
    
    # check values havent changed
    expect_equal(dr[[1]],12.39,tolerance = 1e3)
    expect_equal(dr[[100]],32.54,tolerance = 1e3)
    
    # just number of filtered columns
    expect_true(ncol(predicted(M))==1052)
    expect_true(ncol(DE)-ncol(predicted(M))==527)
    expect_true(ncol(DE)-ncol(predicted(M))==sum(dr>20))
})
