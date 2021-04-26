#' @eval get_description('fisher_exact')
#' @examples
#' # load some data
#' D=MTBLS79_DatasetExperiment()
#'
#' # prepare predictions based on NA
#' pred=as.data.frame(is.na(D$data))
#' pred=lapply(pred,factor,levels=c(TRUE,FALSE))
#' pred=as.data.frame(pred)
#'
#' # apply method
#' M = fisher_exact(alpha=0.05,mtc='fdr',factor_name='Class',factor_pred=pred)
#' M=model_apply(M,D)
#' @import struct
#' @import stats
#' @export fisher_exact
fisher_exact = function(alpha=0.05,mtc='fdr',factor_name,factor_pred,...) {
    out=struct::new_struct('fisher_exact',
        alpha=alpha,
        mtc=mtc,
        factor_name=factor_name,
        factor_pred=factor_pred,
        ...)
    return(out)
}


.fisher_exact<-setClass(
    "fisher_exact",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='enum_stato',
        factor_name='entity',
        factor_pred='entity',

        # OUTPUTS
        p_value='entity_stato',
        significant='entity'
    ),
    prototype = list(name='Fisher Exact Test',
        description=paste0('A fisher exact test is used to analyse ',
        'contingency tables by comparing the number of correctly/incorrectly ',
        'predicted group labels. A multiple test corrected p-value indicates ',
        'whether the number of measured values is significantly different ',
        'between groups. '),
        type="univariate",
        predicted='p_value',
        stato_id="STATO:0000073",
        .params=c('alpha','mtc','factor_name','factor_pred'),
        .outputs=c('p_value','significant'),

        alpha=ents$alpha,
        mtc=ents$mtc,
        factor_name=ents$factor_name,
        factor_pred=entity(name='Predictions',
            value=data.frame(),
            type='data.frame',
            description=paste0('A data.frame, where each column is a factor of predicted ',
            'group labels to compare with the true groups labels.')
        ),
        p_value=ents$p_value,
        significant=ents$significant
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
        names(p)=colnames(M$factor_pred)

        s=p<M$alpha
        names(s)=colnames(M$factor_pred)

        M$p_value=as.data.frame(p)
        M$significant=as.data.frame(s)

        return(M)
    }
)









