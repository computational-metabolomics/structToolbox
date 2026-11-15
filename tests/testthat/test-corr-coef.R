# corr_coef
test_that('corr_coef matches cor.test for all methods',{
    set.seed('57475')
    # DatasetExperiment
    D=iris_DatasetExperiment()
    # two numeric factors so column positions are tested for more than one factor
    D$sample_meta$a=rnorm(150)
    D$sample_meta$b=D$data[,1]+rnorm(150)

    for (m in c('pearson','spearman','kendall')) {
        M=corr_coef(factor_names=c('a','b'),method=m,mtc='none')
        M=suppressWarnings(model_apply(M,D)) # ties warnings for spearman/kendall

        for (f in c('a','b')) {
            ref=lapply(D$data,function(x) suppressWarnings(
                cor.test(x,D$sample_meta[[f]],method=m)))
            expect_equal(M$p_value[[f]],unname(sapply(ref,function(r) r$p.value)))
            expect_equal(M$coeff[[f]],unname(sapply(ref,function(r) r$estimate)))
        }
    }
})
