#' vector nromalisation
#'
#' applies vector normalisation
#' @param ... slots and values for the new object 
#' @return struct object
#' @export vec_norm
#' @import pmp
#' @examples
#' M = vec_norm()
#'
vec_norm = function(...) {
    out=.vec_norm()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.vec_norm<-setClass(
    "vec_norm",
    contains = c('model'),
    slots=c(outputs_normalised='entity',
        outputs_coeff='entity'
    ),
    prototype=list(name = 'Vector normalisation',
        description = 'Normalises each row such that the sum of squares is equal to 1',
        type = 'normalisation',
        predicted='normalised',
        outputs_normalised=entity(name = 'Normalised DatasetExperiment',
            description = 'A DatasetExperiment object containing the normalised data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        outputs_coeff=entity(name = 'Normalisation coefficients',
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

#' @param ... slots and values for the new object 
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("vec_norm","DatasetExperiment"),
    definition=function(M,D){
        M=model_apply(M,D)
        return(M)
    })

#' @param ... slots and values for the new object 
#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("vec_norm","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
        return(M)
    })
