#' fisher_exact class
#'
#' Fisher's exact test (FET). Applies FET for all features in a DatasetExperiment.
#'
#' @slot alpha the p-value threshold to declare a result 'significant'
#' @slot mtc multiple test correction method
#' @slot factor_name the sample_meta column to use
#' @slot factor_pred A data.frame, with a factor of predicted group labels to
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
#' @param ... slots and values for the new object
#' @return struct object
#' @export fisher_exact
fisher_exact = function(...) {
    out=.fisher_exact()
    out=struct::new_struct(out,...)
    return(out)
}


.fisher_exact<-setClass(
    "fisher_exact",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='entity_stato',
        factor_name='entity',
        factor_pred='entity',

        # OUTPUTS
        p_value='entity_stato',
        significant='entity'
    ),
    prototype = list(name='Fisher Exact Test',
        description='Fisher Exact Test applied to each column of a DatasetExperiment.',
        type="univariate",
        predicted='p_value',
        stato_id="STATO:0000073",

        alpha=ents$alpha,
        mtc=ents$mtc,
        factor_name=ents$factor_name,
        factor_pred=entity(name='Factor predictions',
            value=data.frame(),
            type='data.frame',
            description='A data.frame, with a factor of predicted group labels to compare with factor_name. Can be a data frame with a factor of predictions for each feature.'
        ),
        p_value=ents$p_value,
        significant=ents$significant
    )
)

#' @param ... slots and values for the new object
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









