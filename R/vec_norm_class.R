#' @eval get_description('vec_norm')
#' @return struct object
#' @export vec_norm
#' @examples
#' M = vec_norm()
#'
vec_norm = function(...) {
    out=struct::new_struct('vec_norm',...)
    return(out)
}


.vec_norm<-setClass(
    "vec_norm",
    contains = c('model'),
    slots=c(normalised='entity',
        coeff='entity'
    ),
    prototype=list(name = 'Vector normalisation',
        description = paste0(
            'The samples in the data matrix are normalised to account for ',
            'differences in concentration by scaling each sample such that the ',
            'sum of squares is equal to 1.'
            ),
        type = 'normalisation',
        predicted='normalised',
        .outputs=c('normalised','coeff'),
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

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("vec_norm","DatasetExperiment"),
    definition=function(M,D){
        M=model_apply(M,D)
        return(M)
    })

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("vec_norm","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
        return(M)
    })
