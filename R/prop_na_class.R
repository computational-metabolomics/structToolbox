#' prop_na model class
#'
#' prop_na Compares proportion of NA for all features in a dataset
#'
#' @import struct
#' @import stats
#' @export prop_na
prop_na<-setClass(
    "prop_na",
    contains=c('method'),
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_name='entity',
        # OUTPUTS
        outputs.p_value='entity.stato',
        outputs.significant='entity',
        outputs.na_count='entity'
        # CHARTS
        # none
    ),
    prototype = list(name='Fisher\'s exact test to compare number of NA',
        description='Applies Fisher\'s exact test to each feature to indicate whether teh proportion of NA per group is greater than expected, with (optional)
    multiple-testing correction.',
        type="univariate",
        predicted='p_value',

        params.factor_name=entity(name='Factor names',
            type='character',
            description='Names of sample_meta columns to use'
        ),

        params.alpha=entity.stato(name='Confidence level',
            stato.id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params.mtc=entity.stato(name='Multiple Test Correction method',
            stato.id='OBI:0200089',
            value='fdr',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        outputs.p_value=entity.stato(name='p value',
            stato.id='STATO:0000175',
            type='numeric',
            description='the probability of observing the calculated statistic.'
        ),
        outputs.significant=entity(name='Significant features',
            #stato.id='STATO:0000069',
            type='logical',
            description='TRUE if the calculated p-value is less than the supplied threshold (alpha)'
        ),
        outputs.na_count=entity(name='Number of NA',
            #stato.id='STATO:0000069',
            type='numeric',
            description='The number of NA values per group of the chosen factor'
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("prop_na",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        y=dataset.sample_meta(D)[[M$factor_name]]
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
        output$p_value=p.adjust(output$p_value,M$mtc)
        M$p_value=output$p_value
        names(M$p_value)=colnames(X)
        M$significant=M$p_value<M$alpha
        names(M$significant)=colnames(X)
        M$na_count=output[,2:ncol(output),drop=FALSE]

        return(M)
    }
)









