#' Split data into subsets
#'
#' Splits the data into a training and test set.
#' @param p The proportion of samples in the training set.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export split_data
#' @examples
#' M = split_data(p=0.75)
#'
split_data = function(p,...) {
    out=struct::new_struct('split_data',
        p=p,
        ...)
    return(out)
}


.split_data<-setClass(
    "split_data",
    contains = c('model'),
    slots=c(p='entity',
        training='entity',
        testing='entity'
    ),

    prototype=list(name = 'Split data',
        description = 'Splits the data into a training and test set',
        type = 'processing',
        predicted = 'testing',
        .params=c('p'),
        .outputs=c('training','testing'),

        p=entity(name = 'Proportion in training set',
            description = 'The proportion of samples selected for the training set. All other samples will be in assigned to the test set.',
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
        n=floor(nMax*opt$p)
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
