#' @eval get_description('equal_split')
#' @export equal_split
#' @examples
#' D = iris_DatasetExperiment()
#' M = equal_split(factor_name='Species')
#' M = model_apply(M,D)
#' @include split_data_class.R
equal_split = function(p_train = 1,factor_name,...) {
    
    out=struct::new_struct('equal_split',
        p_train = p_train,
        factor_name = factor_name,
        ...)
    
    return(out)
}


.equal_split<-setClass(
    "equal_split",
    contains = c('split_data'),
    slots=c(
        factor_name='entity'
    ),
    
    prototype=list(
        name = 'Equal group sized sampling',
        description = paste0('Samples are randomly chosen from each level ',
            'such that the training set has equal numbers of samples for ',
            'all levels. The number of samples is based on the input ',
            'proportion and the smallest group size.'),
        type = 'processing',
        predicted = 'testing',
        .params=c('factor_name'),
        
        factor_name=ents$factor_name
        
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("equal_split","DatasetExperiment"),
    definition=function(M,D) {
        opt=param_list(M)
        
        # stratified resampling
        L = levels(D$sample_meta[[M$factor_name]])
        
        # smallest group size
        S=summary(D$sample_meta[[M$factor_name]])
        w=which.min(S)
        nMax=S[w]
        n=max(c(1,floor(nMax*M$p_train))) # ensure at least one is selected
        
        in_training = numeric(0)
        for (k in L) {
            # indices of this group
            w=which(D$sample_meta[[M$factor_name]]==k)
            # random indices
            idx=sample(x=1:nMax,size = n, replace=FALSE, prob=NULL)
            # collect selected indices
            in_training=c(in_training,w[idx])
        }
        
        
        training=DatasetExperiment(data=D$data[in_training,,drop=FALSE],
            sample_meta=D$sample_meta[in_training,,drop=FALSE],
            variable_meta=D$variable_meta,
            name=c(D$name,'(Training set)'),
            description=c(D$description,'A subset of the data has been selected as a training set'))
        testing=DatasetExperiment(data=D$data[-in_training,,drop=FALSE],
            sample_meta=D$sample_meta[-in_training,,drop=FALSE],
            variable_meta=D$variable_meta,
            name=c(D$name,'(Testing set)'),
            description=c(D$description,'A subset of the data has been selected as a test set'))
        output_value(M,'training')=training
        output_value(M,'testing')=testing
        
        return(M)
    }
)
