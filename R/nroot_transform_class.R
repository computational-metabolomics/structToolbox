#' @eval get_description('nroot_transform')
#' @export nroot_transform
#' @examples
#' M = nroot_transform()
nroot_transform = function(root=2,...) {
    out=struct::new_struct('nroot_transform',
        root=2,
        ...)
    return(out)
}


.nroot_transform<-setClass(
    "nroot_transform",
    contains = c('model'),
    slots=c(root='entity',
        transformed='entity'
    ),
    
    prototype=list(name = 'nth root transform',
        description = 'All values in the data matrix are transformed by raising them to the power of 1/n.',
        type = 'transform',
        predicted = 'transformed',
        .params=c('root'),
        .outputs=c('transformed'),
        
        root=entity(name = 'nth root',
            description = 'The nth root used for the transform.',
            value = 2,
            type='numeric'),
        
        transformed=entity(name = 'nth root transformed DatasetExperiment',
            description = 'A DatasetExperiment object containing the nth root transformed data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("nroot_transform","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)
        
        smeta=D$sample_meta
        x=D$data
        
        out = x^(1/M$root)
        D$data = as.data.frame(out)
        
        output_value(M,'transformed') = D
        
        return(M)
    }
)
