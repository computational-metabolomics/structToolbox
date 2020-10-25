#' @eval get_description('stratified_split')
#' @export stratified_split
#' @examples
#' D = iris_DatasetExperiment()
#' M = stratified_split(p_train=0.75,factor_name='Species')
#' M = model_apply(M,D)
#'
stratified_split = function(p_train,factor_name,...) {
    
    out=struct::new_struct('stratified_split',
        p_train = p_train,
        factor_name = factor_name,
        ...)
    
    return(out)
}


.stratified_split<-setClass(
    "stratified_split",
    contains = c('split_data'),
    slots=c(
        factor_name='entity'
    ),
    
    prototype=list(
        name = 'Stratified sampling',
        description = paste0('The dataset is divided into two subsets. A ',
        'predefined proportion of samples from each level of a factor is ',
        'selected for the training set, and the remaining samples are used for ',
        'the test set. The stratification by factor level means that the ',
        'relative number of samples per level is approximately equal to the ',
        'original dataset.'),
        type = 'processing',
        predicted = 'testing',
        .params=c('factor_name'),
        
        factor_name=ents$factor_name

    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("stratified_split","DatasetExperiment"),
    definition=function(M,D) {
        opt=param_list(M)
        
            # stratified resampling
            L = levels(D$sample_meta[[M$factor_name]])
            
            in_training = numeric(0)
            for (k in L) {
                # indices of this group
                w=which(D$sample_meta[[M$factor_name]]==k)
                # size of this group
                nMax=length(w)
                # number in the training set
                n=floor(nMax*M$p_train)
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
