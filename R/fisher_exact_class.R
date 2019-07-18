#' fisher_exact class
#'
#' Fisher's exact test (FET). Applies FET for all features in a dataset.
#'
#' @param alpha the p-value threshold to declare a result 'significant'
#' @param mtc multiple test correction method
#' @param factor_name the sample_meta column to use
#' @param factor_pred A data.frame, with a factor of predicted group labels to
#' compare with factor_name. Can be a data frame with a factor of predictions
#' for each feature.'
#'
#' @examples
#' # load some data
#' D=sbcms_dataset()
#'
#' # prepare predictions based on NA
#' pred=as.data.frame(is.na(D$data))
#' pred=lapply(pred,factor,levels=c(TRUE,FALSE))
#' pred=as.data.frame(pred)
#'
#' # apply method
#' M = fisher_exact(alpha=0.05,mtc='fdr',factor_name='class',factor_pred=pred)
#' M=method.apply(M,D)
#'
#' @import struct
#' @import stats
#' @export fisher_exact
fisher_exact<-setClass(
    "fisher_exact",
    contains=c('method','stato'),
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_name='entity',
        params.factor_pred='entity',
        # OUTPUTS
        outputs.p_value='entity.stato',
        outputs.significant='entity'
    ),
    prototype = list(name='Fisher Exact Test',
        description='Fisher Exact Test applied to each column of a dataset.',
        type="univariate",
        predicted='p_value',
        stato.id="STATO:0000073",

        params.alpha=entity.stato(name='Confidence level',
            stato.id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params.mtc=entity.stato(name='Multiple Test Correction method',
            stato.id='OBI:0200089',
            value='fdr',
            type='numeric',
            description='The method used to adjust for multiple comparisons.'
        ),
        params.factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The column name of meta data to use.'
        ),
        params.factor_pred=entity(name='Factor predictions',
            value='factor',
            type='data.frame',
            description='A data.frame, with a factor of predicted group labels to compare with factor_name. Can be a data frame with a factor of predictions for each feature.'
        ),
        outputs.p_value=entity.stato(name='p value',
            stato.id='STATO:0000175',
            type='numeric',
            description='the probability of observing the calculated t-statistic.'
        ),
        outputs.significant=entity(name='Significant features',
            #stato.id='STATO:0000069',
            type='logical',
            description='TRUE if the calculated p-value is less than the supplied threshold (alpha)'
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("fisher_exact",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)

        FET=lapply(M$factor_pred,function(x) {
            ft=fisher.test(x=D$sample_meta[[M$factor_name]],y=x)
        }
        )

        p=sapply(FET,function(x) return(x$p.value))
        p=p.adjust(p,M$mtc)
        names(p)=colnames(X)

        s=p<M$alpha
        names(s)=colnames(X)

        M$p_value=p
        M$significant=s

        return(M)
    }
)









