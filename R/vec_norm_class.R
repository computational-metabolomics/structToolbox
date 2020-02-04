#' vector nromalisation
#'
#' applies vector normalisation
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export vec_norm
#' @import pmp
#' @examples
#' M = vec_norm()
#'
vec_norm = function(...) {
    out=.vec_norm()
    out=struct::new_struct(out,...)
    return(out)
}


.vec_norm<-setClass(
    "vec_norm",
    contains = c('model'),
    slots=c(normalised='entity',
        coeff='entity'
    ),
    prototype=list(name = 'Vector normalisation',
        description = 'Normalises each row such that the sum of squares is equal to 1',
        type = 'normalisation',
        predicted='normalised',
        normalised=entity(name = 'Normalised DatasetExperiment',
            description = 'A DatasetExperiment object containing the normalised data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        coeff=entity(name = 'Normalisation coefficients',
            description = 'The normalisation coefficients calculated by PQN',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("vec_norm","DatasetExperiment"),
    definition=function(M,D)
    {
        smeta=D$sample_meta
        x=D$data

        coeff = apply(x,MARGIN = 2,FUN = function(z) sqrt(sum(z, na.rm = TRUE)))
        normalised = apply(x,MARGIN = 2,FUN = function(z) z/sqrt(sum(z, na.rm=TRUE)))
        D$data = as.data.frame(normalised)

        output_value(M,'normalised') = D
        output_value(M,'coeff') = data.frame('coeff'=coeff)

        return(M)
    }
)

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("vec_norm","DatasetExperiment"),
    definition=function(M,D){
        M=model_apply(M,D)
        return(M)
    })

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("vec_norm","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
        return(M)
    })
