#' log transform
#'
#' applies a log transform to the input data
#' @param ... slots and values for the new object 
#' @return struct object
#' @export log_transform
#' @examples
#' M = log_transform()
log_transform = function(...) {
    out=.log_transform()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.log_transform<-setClass(
    "log_transform",
    contains = c('model'),
    slots=c(params_base='entity',
        outputs_transformed='entity'
    ),

    prototype=list(name = 'logarithm transform',
        description = 'applies a log tranform to the data.',
        type = 'transform',
        predicted = 'transformed',

        params_base=entity(name = 'logarithm base',
            description = 'The base of the logarithm used for the tranform.',
            value = 10,
            type='numeric'),

        outputs_transformed=entity(name = 'log transformed DatasetExperiment',
            description = 'A DatasetExperiment object containing the log transformed data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        )
    )
)

#' @param ... slots and values for the new object 
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
