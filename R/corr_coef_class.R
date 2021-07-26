#' @eval get_description('corr_coef')
#' @examples
#' D = MTBLS79_DatasetExperiment(filtered=TRUE)
#'
#' # subset for this example
#' D = D[,1:10]
#'
#' # convert to numeric for this example
#' D$sample_meta$sample_order=as.numeric(D$sample_meta$run_order)
#' D$sample_meta$sample_rep=as.numeric(D$sample_meta$Sample_Rep)
#'
#' M = corr_coef(factor_names=c('sample_order','sample_rep'))
#' M = model_apply(M,D)
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
        alpha='entity',
        mtc='enum',
        factor_names='entity',
        method='enum',
        # OUTPUTS
        coeff='entity',
        p_value='entity',
        significant='entity'
    ),
    prototype = list(name='Correlation coefficient',
        description=paste0(
            'The correlation between features and a set of ',
            'continuous factor are calculated. Multiple-test corrected ',
            'p-values are used to indicate whether the computed coefficients ',
            'may have occurred by chance.'),
        type="univariate",
        libraries='stats',
        ontology='STATO:0000142',
        predicted='p_value',
        .params=c('alpha','mtc','factor_names','method'),
        .outputs=c('coeff','p_value','significant'),

        factor_names=ents$factor_names,
        alpha=ents$alpha,
        mtc=ents$mtc,

        method=enum(name='Type of correlation',
            value='spearman',
            type='character',
            description=c(
                "kendall" = "Kendall's tau is computed.",
                "pearson" = 'Pearson product moment correlation is computed.',
                "spearman" = "Spearman's rho statistic is computed."
                ),
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

        p_val=as.data.frame(vapply(
                out[,seq(2,ncol(out),7),drop=FALSE],
                FUN.VALUE = numeric(ncol(D)),
                FUN=p.adjust,method=M$mtc),
            row.names = colnames(D$data))
        
        colnames(p_val)=M$factor_names
        M$p_value=p_val

        coeff=as.data.frame(out[,seq(3,ncol(out),7),drop=FALSE],row.names = colnames(D$data))
        colnames(coeff)=M$factor_names
        M$coeff=coeff

        M$significant=data.frame('significant'=M$p_value<M$alpha,row.names = colnames(D$data))

        return(M)
    }
)









