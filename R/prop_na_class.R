#' prop_na model class
#'
#' prop_na Compares proportion of NA for all features in a DatasetExperiment
#'
#' @import struct
#' @import stats
#' @export prop_na
#' @examples
#' M = prop_na()
#'
prop_na = function(...) {
    out=.prop_na()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.prop_na<-setClass(
    "prop_na",
    contains=c('model'),
    slots=c(
        # INPUTS
        params_alpha='entity_stato',
        params_mtc='entity_stato',
        params_factor_name='entity',
        # OUTPUTS
        outputs_p_value='entity_stato',
        outputs_significant='entity',
        outputs_na_count='entity'
        # CHARTS
        # none
    ),
    prototype = list(name='Fisher\'s exact test to compare number of NA',
        description='Applies Fisher\'s exact test to each feature to indicate whether teh proportion of NA per group is greater than expected, with (optional)
    multiple-testing correction.',
        type="univariate",
        predicted='p_value',

        params_factor_name=entity(name='Factor names',
            type='character',
            description='Names of sample_meta columns to use'
        ),

        params_alpha=entity_stato(name='Confidence level',
            stato_id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params_mtc=entity_stato(name='Multiple Test Correction method',
            stato_id='OBI:0200089',
            value='fdr',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        outputs_p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated statistic.'
        ),
        outputs_significant=entity(name='Significant features',
            #stato_id='STATO:0000069',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threshold (alpha)'
        ),
        outputs_na_count=entity(name='Number of NA',
            #stato_id='STATO:0000069',
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









