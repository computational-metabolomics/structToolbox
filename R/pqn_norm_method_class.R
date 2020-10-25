#' @eval get_description('pqn_norm')
#' @export pqn_norm
#' @examples
#' D = iris_DatasetExperiment()
#' M = pqn_norm(factor_name='Species',qc_label='all')
#' M = model_apply(M,D)
#'
pqn_norm = function(qc_label='QC',factor_name,...) {
    out=struct::new_struct('pqn_norm',
        qc_label=qc_label,
        factor_name=factor_name,
        ...)
    return(out)
}


.pqn_norm<-setClass(
    "pqn_norm",
    contains = c('model'),
    slots=c(qc_label='entity',
        factor_name='entity',
        normalised='entity',
        coeff='entity'
    ),
    prototype=list(name = 'Probabilistic Quotient Normalisation (PQN)',
        description = paste0(
            'PQN is used to normalise for differences in concentration ',
            'between samples. It makes use of Quality Control (QC) samples as ',
            'a reference. PQN scales by the median change relative to the ',
            'reference in order to be more robust against changes caused by ',
            'response to perturbation.'),
        type = 'normalisation',
        predicted = 'normalised',
        libraries='pmp',
        .params=c('qc_label','factor_name'),
        .outputs=c('normalised','coeff'),

        qc_label=entity(name = 'QC label',
            description = 'The label used to identify QC samples.',
            value = 'QC',
            type='character'),

        normalised=entity(name = 'Normalised DatasetExperiment',
            description = 'A DatasetExperiment object containing the normalised data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        coeff=entity(name = 'PQN coefficients',
            description = 'The normalisation coefficients calculated by PQN',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("pqn_norm","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        normalised = pmp::pqn_normalisation(t(x), classes=smeta[,M$factor_name],qc_label=opt$qc_label) # operates on transpose of x
        D$data = as.data.frame(t(normalised))

        output_value(M,'normalised') = D
        output_value(M,'coeff') = data.frame('coeff'=attributes(normalised)$flags,row.names = rownames(x))

        return(M)
    }
)


##### plots
#' @eval get_description('pqn_norm_hist')
#' @import struct
#' @export pqn_norm_hist
#' @examples
#' C = pqn_norm_hist()
pqn_norm_hist = function(...) {
    out=struct::new_struct('pqn_norm_hist',...)
    return(out)
}

.pqn_norm_hist<-setClass(
    "pqn_norm_hist",
    contains='chart',
    prototype = list(name='PQN coefficient histogram',
        description='A histogram of the PQN coefficients for all features',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pqn_norm_hist",'pqn_norm'),
    definition=function(obj,dobj)
    {
        A=output_value(dobj,'coeff')

        out=ggplot(data=A, aes_(x=~pqn_coef)) +
            geom_histogram(color='white') +
            xlab('PQN coefficient') +
            ylab('Count') +
            scale_fill_Publication()+
            theme_Publication(base_size = 12)

        return(out)
    }
)
