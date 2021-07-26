#' @eval get_description('log_transform')
#' @return struct object
#' @export log_transform
#' @examples
#' M = log_transform()
log_transform = function(base=10,...) {
    out=struct::new_struct('log_transform',
        base=base,
        ...)
    return(out)
}


.log_transform<-setClass(
    "log_transform",
    contains = c('model'),
    slots=c(base='entity',
        transformed='entity'
    ),

    prototype=list(name = 'logarithm transform',
        description = 'A logarithmic transform is applied to all values in the data matrix.',
        type = 'transform',
        predicted = 'transformed',
        .params=c('base'),
        .outputs=c('transformed'),
        ontology = 'OBI:0200094',
        base=entity(name = 'Logarithm base',
            description = 'The base of the logarithm used for the transform.',
            value = 10,
            type='numeric'),

        transformed=entity(name = 'Log transformed DatasetExperiment',
            description = 'A DatasetExperiment object containing the log transformed data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("log_transform","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        out = log(x,base = opt$base)
        D$data = as.data.frame(out)

        output_value(M,'transformed') = D

        return(M)
    }
)
