#' @eval get_description('constant_sum_norm')
#' @export constant_sum_norm
#' @examples
#' M = constant_sum_norm()
#'
constant_sum_norm = function(scaling_factor=1,...) {
    out=struct::new_struct('constant_sum_norm',
        scaling_factor=scaling_factor,
        ...)
    return(out)
}


.constant_sum_norm<-setClass(
    "constant_sum_norm",
    contains = c('model'),
    slots=c(
        scaling_factor='entity',
        normalised='entity',
        coeff='entity'
    ),
    prototype=list(name = 'Normalisation to constant sum',
        description = paste0('Each sample is normalised such that the total ',
            'signal is equal to one (or a scaling factor if specified).'),
        type = 'normalisation',
        predicted='normalised',
        ontology='OBI:0200026',
        .params='scaling_factor',
        .outputs=c('normalised','coeff'),
        scaling_factor=entity(name = 'Normalised total',
            description = 'The scaling factor applied after normalisation.',
            type='numeric',
            value=1
        ),
        normalised=entity(name = 'Normalised DatasetExperiment',
            description = 'A DatasetExperiment object containing the normalised data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        coeff=entity(name = 'Normalisation coefficients',
            description = 'The sum of each row, used to normalise the samples.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("constant_sum_norm","DatasetExperiment"),
    definition=function(M,D)
    {
        smeta=D$sample_meta
        x=D$data
        
        coeff = apply(x,MARGIN = 1,FUN = function(z) sum(z, na.rm = TRUE))
        normalised = apply(x,MARGIN = 1,FUN = function(z) {
            (z/sum(z, na.rm=TRUE))*M$scaling_factor})
        D$data = as.data.frame(t(normalised))
        
        output_value(M,'normalised') = D
        output_value(M,'coeff') = data.frame('coeff'=coeff)
        
        return(M)
    }
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("constant_sum_norm","DatasetExperiment"),
    definition=function(M,D){
        M=model_apply(M,D)
        return(M)
    })

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("constant_sum_norm","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
        return(M)
    })
