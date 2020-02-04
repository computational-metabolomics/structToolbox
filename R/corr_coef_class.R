#' Correlation Coefficient
#'
#' Calculates correlation between features and continuous variables.
#'
#' @import struct
#' @import stats
#' @examples
#' M = corr_coef()
#' @templateVar paramNames c('alpha','mtc','factor_names')
#' @template common_params
#' @param factor_names Sample_meta column names to correlate features with
#' @param method 'Calculate "kendall", "pearson" or "spearman" correlation coefficient. Default method = "spearman".'
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export
corr_coef = function(alpha=0.05,mtc='fdr',factor_names,method='spearman',...) {
    out=struct::new_struct('corr_coef',
        alpha=alpha,
        mtc=mtc,
        factor_names=factor_names,
        method=method,
        ...)
    return(out)
}

.corr_coef<-setClass(
    "corr_coef",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='entity_stato',
        factor_names='entity',
        method='enum',
        # OUTPUTS
        coeff='entity',
        p_value='entity',
        significant='entity'
    ),
    prototype = list(name='Correlation coefficient',
        description='Calculates the correlation coefficient between features and continuous factors.',
        type="univariate",
        predicted='p_value',
        .params=c('alpha','mtc','factor_names','method'),
        .outputs=c('coeff','p_value','significant'),

        factor_names=ents$factor_names,
        alpha=ents$alpha,
        mtc=ents$mtc,

        method=enum(name='Type of correlation',
            value='spearman',
            type='character',
            description='"kendall", "pearson" or "spearman" correlation coefficient. Default="spearman".',
            allowed=c("kendall", "pearson","spearman")
        ),
        coeff=entity(name='Correlation coefficient',
            type='data.frame',
            description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.',
            value=data.frame()
        ),
        p_value=ents$p_value,
        significant=ents$significant
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









