#' @eval get_description('mean_of_medians')
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_of_medians(factor_name='Species')
#' M = model_apply(M,D)
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
        description='The data matrix is normalised by the mean of the median of each factor level.',
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
        for (k in levels(D$sample_meta[[M$factor_name]])) {
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


