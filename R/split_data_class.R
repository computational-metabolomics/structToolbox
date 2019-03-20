#' split data into sets
#'
#' Splits the data into a training and test set
#' @export split_data
split_data<-setClass(
  "split_data",
  contains = c('method'),
  slots=c(params.p='entity',
          outputs.training='entity',
          outputs.testing='entity'
  ),

  prototype=list(name = 'Split data',
                 description = 'Splits the data into a training and test set',
                 type = 'processing',
                 predicted = 'testing',
                 params=c('p'),
                 outputs=c('training','testing'),

                 params.p=entity(name = 'Proportion in training set',
                                        description = 'The proportion of samples selected for the training set. All other samples willbe in assigned to the test set.',
                                        value = 0.75,
                                        type='numeric'),

                 outputs.training=entity(name = 'A dataset of training data',
                                            description = 'A dataset object containing samples selected for the training set.',
                                            type='dataset',
                                            value=dataset()
                 ),
                 outputs.testing=entity(name = 'A dataset of data for testing',
                                         description = 'A dataset object containing samples selected for the testing set.',
                                         type='dataset',
                                         value=dataset()
                 )
  )
)

#' @export
setMethod(f="method.apply",
          signature=c("split_data","dataset"),
          definition=function(M,D)
          {
            opt=param.list(M)
            # number of samples
            nMax=nrow(dataset.data(D))
            # number in the training set
            n=floor(nMax*opt$p)
            # select a random subset of the data for training
            in_training=sample(x=1:nMax,size = n, replace=FALSE,prob=NULL)
            training=dataset(data=dataset.data(D)[in_training,,drop=FALSE],
                             sample_meta=dataset.sample_meta(D)[in_training,,drop=FALSE],
                             variable_meta=dataset.variable_meta(D),
                             name=c(name(D),'(Training set)'),
                             description=c(description(D),'A subset of the data has been selected as a training set'))
            testing=dataset(data=dataset.data(D)[-in_training,,drop=FALSE],
                             sample_meta=dataset.sample_meta(D)[-in_training,,drop=FALSE],
                             variable_meta=dataset.variable_meta(D),
                             name=c(name(D),'(Testing set)'),
                             description=c(description(D),'A subset of the data has been selected as a test set'))
            output.value(M,'training')=training
            output.value(M,'testing')=testing

            return(M)
          }
)
