#' @eval get_description('prop_na')
#' @return struct object
#' @export prop_na
#' @examples
#' M = prop_na(factor_name='Species')
#'
prop_na = function(alpha=0.05,mtc='fdr',factor_name,...) {
    out=struct::new_struct('prop_na',
        alpha=alpha,
        mtc=mtc,
        factor_name=factor_name,
        ...)
    return(out)
}


.prop_na<-setClass(
    "prop_na",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity',
        mtc='enum',
        factor_name='entity',
        # OUTPUTS
        p_value='entity',
        significant='entity',
        na_count='entity'
        # CHARTS
        # none
    ),
    prototype = list(name='Fisher\'s exact test for missing values',
        description=paste0('A Fisher\'s exact test is used to compare the ',
        'number of missing values in each group. Multiple test corrected ',
        'p-values are computed to indicate whether there is a significant ',
        'difference in the number of missing values across groups for each ',
        'feature.'),
        type="univariate",
        predicted='p_value',
        .params=c('alpha','mtc','factor_name'),
        .outputs=c('p_value','significant','na_count'),

        factor_name=ents$factor_name,

        alpha=ents$alpha,
        mtc=ents$mtc,
        p_value=entity(name='p value',
            ontology='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated statistic.'
        ),
        significant=entity(name='Significant features',
            #ontology='STATO:0000069',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threshold (alpha)'
        ),
        na_count=entity(name='Number of NA',
            #ontology='STATO:0000069',
            type='data.frame',
            description='The number of NA values per group of the chosen factor'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("prop_na",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        y=D$sample_meta[[M$factor_name]]
        L=levels(y)
        output=apply(X,2,function(x) {
            na_count=numeric(length(L))
            names(na_count)=L

            for (j in L) {
                na_count[j]=sum(is.na(x[y==j]))
            }

            if (all(na_count==0)) {
                s=list('p_value'=1)
            } else {
                s=fisher.test(is.na(x),y,alternative='greater') # is the number of NA greater than expected?
            }

            d=data.frame('p_value'=s$p.value)
            temp=as.data.frame(na_count)
            temp=cbind(d,t(as.matrix(na_count)))
            temp=t(temp)
            return(temp)
        })
        rownames(output)=c('p_value',L)
        output=as.data.frame(t(output))
        # fdr correction
        output$p_value=data.frame(p_value=p.adjust(output$p_value,M$mtc))
        M$p_value=output$p_value
        rownames(M$p_value)=colnames(X)
        M$significant=as.data.frame(M$p_value<M$alpha)
        rownames(M$significant)=colnames(X)
        M$na_count=as.data.frame(output[,2:ncol(output),drop=FALSE])

        return(M)
    }
)









