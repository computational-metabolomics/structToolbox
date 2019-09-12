#' correlation model class
#'
#' correlation model class. Calculate correlation between features and continuous variables
#'
#' @import struct
#' @import stats
#' @export corr_coef
corr_coef<-setClass(
    "corr_coef",
    contains=c('method'),
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_names='entity',
        params.method='enum',
        # OUTPUTS
        outputs.coeff='entity',
        outputs.p_value='entity',
        outputs.significant='entity'
    ),
    prototype = list(name='Correlation coefficient',
        description='Calculates the correlation coefficient between features and continuous factors.',
        type="univariate",
        predicted='p_value',

        params.factor_names=entity(name='Factor names',
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

        params.method=enum(name='Type of correlation',
            value='spearman',
            type='character',
            description='"kendall", "pearson" or "spearman" correlation coefficient. Default="spearman".',
            list=c("kendall", "pearson","spearman")
        ),
        outputs.coeff=entity(name='Correlation coefficient',
            type='data.frame',
            description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.'
        ),
        outputs.p_value=entity.stato(name='p value',
            stato.id='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated t-statistic.'
        ),

        outputs.significant=entity(name='Significant features',
            #stato.id='STATO:0000069',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("corr_coef",'dataset'),
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

        M$p_value=data.frame('p_value'=p.adjust(as.numeric(out[,2]),method=M$mtc),row.names = colnames(D$data))
        M$coeff=data.frame('coeff'=as.numeric(out[,3]),row.names = colnames(D$data))
        M$significant=data.frame('significant'=M$p_value<M$alpha,row.names = colnames(D$data))

        return(M)
    }
)









