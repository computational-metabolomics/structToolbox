#' Mean of median adjustment
#'
#' Applies an offset to the data such that the mean of the medians is equal for all samples.
#' @param factor_name the column sample of sample_meta to use. Mean of medians 
#' will be applied based on the levels in this factor.
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_of_medians(factor_name='Species')
#' M = model_apply(M,D)
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export
mean_of_medians = function(factor_name,...) {
    out=struct::new_struct('mean_of_medians',
        factor_name=factor_name,
        ...)
    return(out)
}

.mean_of_medians<-setClass(
    "mean_of_medians",
    contains='model',
    slots=c(
        'factor_name' = 'entity',
        'transformed' = 'entity'
    ),
    prototype = list(
        name='Mean of medians',
        type="preprocessing",
        predicted='transformed',
        .params=c('factor_name'),
        .outputs=c('transformed'),
        factor_name = ents$factor_name,
        transformed=entity(
            name = 'Transformed data matrix', 
            description=c('Data after the tranformation has been applied.'),
            value=DatasetExperiment(),
            type='DatasetExperiment'
        )
        
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("mean_of_medians",'DatasetExperiment'),
    definition=function(M,D){
        # sample medians
        m = apply(D$data,1,median,na.rm=TRUE)
        
        # difference between medians means of medians each factor level
        md=numeric(length(m))
        for (k in levels(M$factor_name)) {
            mm = mean(m[D$sample_meta[[M$factor_name]]==k]) # mean of medians in this factor level
            md[D$sample_meta[[M$factor_name]]==k]=m[D$sample_meta[[M$factor_name]]==k] - mm
        }
        
        # subtract from data matrix
        md = matrix(md,nrow=nrow(D$data),ncol=ncol(D$data),byrow = FALSE)
        D$data=D$data - md
        
        # set outputs
        M$transformed=D
        
        return(M)
    }
)


