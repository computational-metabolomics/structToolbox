#' correlation model class
#'
#' correlation model class. Calculate correlation between features and continuous variables
#'
#' @import struct
#' @import stats
#' @examples
#' M = corr_coef()
#'
#' @export corr_coef
corr_coef = function(...) {
    out=.corr_coef()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.corr_coef<-setClass(
    "corr_coef",
    contains=c('model'),
    slots=c(
        # INPUTS
        params_alpha='entity_stato',
        params_mtc='entity_stato',
        params_factor_names='entity',
        params_method='enum',
        # OUTPUTS
        outputs_coeff='entity',
        outputs_p_value='entity',
        outputs_significant='entity'
    ),
    prototype = list(name='Correlation coefficient',
        description='Calculates the correlation coefficient between features and continuous factors.',
        type="univariate",
        predicted='p_value',

        params_factor_names=entity(name='Factor names',
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

        params_method=enum(name='Type of correlation',
            value='spearman',
            type='character',
            description='"kendall", "pearson" or "spearman" correlation coefficient. Default="spearman".',
            list=c("kendall", "pearson","spearman")
        ),
        outputs_coeff=entity(name='Correlation coefficient',
            type='data.frame',
            description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.'
        ),
        outputs_p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated t-statistic.'
        ),

        outputs_significant=entity(name='Significant features',
            #stato_id='STATO:0000069',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("corr_coef",'DatasetExperiment'),
    definition=function(M,D)
    {

        fcn2=function(y,x) {
            s=cor.test(x,y,method=M$method,use="na.or.complete")
        }
        fcn=function(x) {
            X=D$sample_meta[M$factor_names]
            out=unlist(lapply(X,fcn2,x))
        }

        out=apply(D$data,2,fcn)
        out=as.data.frame(t(out),stringsAsFactors = FALSE)
        out=as.data.frame(lapply(out,as.numeric))

        p_val=as.data.frame(lapply(out[,seq(2,ncol(out),7)],p.adjust,method=M$mtc),row.names = colnames(D$data))
        colnames(p_val)=M$factor_names
        M$p_value=p_val

        coeff=as.data.frame(out[,seq(3,ncol(out),7),drop=FALSE],row.names = colnames(D$data))
        colnames(coeff)=M$factor_names
        M$coeff=coeff

        M$significant=data.frame('significant'=M$p_value<M$alpha,row.names = colnames(D$data))

        return(M)
    }
)









