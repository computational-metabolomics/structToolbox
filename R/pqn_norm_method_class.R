#' PQN nromalisation
#'
#' applies PQN normalisation using QC samples as reference samples
#' @param ... slots and values for the new object 
#' @return struct object
#' @export pqn_norm
#' @import pmp
#' @examples
#' M = pqn_norm()
pqn_norm = function(...) {
    out=.pqn_norm()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pqn_norm<-setClass(
    "pqn_norm",
    contains = c('model'),
    slots=c(params_qc_label='entity',
        params_factor_name='entity',
        outputs_normalised='entity',
        outputs_coeff='entity'
    ),
    prototype=list(name = 'Probabilistic Quotient Normalisation (PQN)',
        description = 'PQN normalisation using QC samples as reference samples',
        type = 'normalisation',
        predicted = 'normalised',

        params_qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        outputs_normalised=entity(name = 'Normalised DatasetExperiment',
            description = 'A DatasetExperiment object containing the normalised data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        outputs_coeff=entity(name = 'PQN coefficients',
            description = 'The normalisation coefficients calculated by PQN',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("pqn_norm","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        normalised = pqn_normalisation(t(x), classes=smeta[,M$factor_name],qc_label=opt$qc_label) # operates on transpose of x
        D$data = as.data.frame(t(normalised$df))

        output_value(M,'normalised') = D
        output_value(M,'coeff') = data.frame('coeff'=normalised$coef,row.names = rownames(x))

        return(M)
    }
)


##### plots
#' plot for PQN normalisation
#'
#' plots a histogram of the PQN coeffients
#' @import struct
#' @param ... slots and values for the new object 
#' @return struct object
#' @export pqn_norm_hist
#' @examples
#' C = pqn_norm_hist()
pqn_norm_hist = function(...) {
    out=.pqn_norm_hist()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pqn_norm_hist<-setClass(
    "pqn_norm_hist",
    contains='chart',
    prototype = list(name='Histogram of the PQN coefficients per feature',
        description='A histogram of the PQN coefficients per feature',
        type="histogram"
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pqn_norm_hist",'pqn_norm'),
    definition=function(obj,dobj)
    {
        A=output_value(dobj,'coeff')

        out=ggplot(data=A, aes_(x=~coeff)) +
            geom_histogram(color='white') +
            xlab('PQN coefficient') +
            ylab('Count') +
            scale_fill_Publication()+
            theme_Publication(base_size = 12)

        return(out)
    }
)
