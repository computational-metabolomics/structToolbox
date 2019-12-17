#' fisher_exact class
#'
#' Fisher's exact test (FET). Applies FET for all features in a DatasetExperiment.
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
#' D=sbcms_DatasetExperiment()
#'
#' # prepare predictions based on NA
#' pred=as.data.frame(is.na(D$data))
#' pred=lapply(pred,factor,levels=c(TRUE,FALSE))
#' pred=as.data.frame(pred)
#'
#' # apply method
#' M = fisher_exact(alpha=0.05,mtc='fdr',factor_name='class',factor_pred=pred)
#' M=model_apply(M,D)
#'
#' @import struct
#' @import stats
#' @export fisher_exact
fisher_exact = function(...) {
    out=.fisher_exact()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.fisher_exact<-setClass(
    "fisher_exact",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        params_alpha='entity_stato',
        params_mtc='entity_stato',
        params_factor_name='entity',
        params_factor_pred='entity',

        # OUTPUTS
        outputs_p_value='entity_stato',
        outputs_significant='entity'
    ),
    prototype = list(name='Fisher Exact Test',
        description='Fisher Exact Test applied to each column of a DatasetExperiment.',
        type="univariate",
        predicted='p_value',
        stato_id="STATO:0000073",

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
        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The column name of meta data to use.'
        ),
        params_factor_pred=entity(name='Factor predictions',
            value=data.frame(),
            type='data.frame',
            description='A data.frame, with a factor of predicted group labels to compare with factor_name. Can be a data frame with a factor of predictions for each feature.'
        ),
        outputs_p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='numeric',
            description='the probability of observing the calculated t-statistic.',
            value=-1
        ),
        outputs_significant=entity(name='Significant features',
            #stato_id='STATO:0000069',
            type='logical',
            description='TRUE if the calculated p-value is less than the supplied threshold (alpha)'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("fisher_exact",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data


        FET=lapply(M$factor_pred,function(x) {
            if (all(x==TRUE) || all(x==FALSE)){
                ft=list()
                ft$p.value=NA
                return(ft)
            }
            ft=fisher.test(x=D$sample_meta[[M$factor_name]],y=x,simulate.p.value = TRUE)
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









