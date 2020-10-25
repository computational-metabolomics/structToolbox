#' @eval get_description('split_data')
#' @export split_data
#' @examples
#' M = split_data(p_train=0.75)
#'
split_data = function(p_train,...) {
    out=struct::new_struct('split_data',
        p_train=p_train,
        ...)
    return(out)
}


.split_data<-setClass(
    "split_data",
    contains = c('model'),
    slots=c(p_train='entity',
        training='entity',
        testing='entity'
    ),

    prototype=list(
        name = 'Split data',
        description = paste0('The data matrix is divided into two subsets.',
        'A predefined proportion of the samples are randomly selected for a ',
        'training set, and the remaining samples are used for the test set.'),
        type = 'processing',
        predicted = 'testing',
        .params=c('p_train'),
        .outputs=c('training','testing'),

        p_train=entity(name = 'Proportion in training set',
            description = paste0('The proportion of samples selected for the ',
            'training set.'),
            value = 0.75,
            type='numeric'),

        training=entity(name = 'A DatasetExperiment of training data',
            description = 'A DatasetExperiment object containing samples selected for the training set.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        testing=entity(name = 'A DatasetExperiment of data for testing',
            description = 'A DatasetExperiment object containing samples selected for the testing set.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("split_data","DatasetExperiment"),
    definition=function(M,D) {
        opt=param_list(M)
        # number of samples
        nMax=nrow(D$data)
        # number in the training set
        n=floor(nMax*opt$p_train)
        # select a random subset of the data for training
        in_training=sample(x=1:nMax,size = n, replace=FALSE,prob=NULL)
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
