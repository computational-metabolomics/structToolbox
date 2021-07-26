#' @eval get_description('bootstrap')
#' @export bootstrap
#' @examples
#' I = bootstrap(number_of_repetitions = 10, collect = 'vip')
bootstrap = function(number_of_repetitions = 100, collect, ...) {
    out=struct::new_struct('bootstrap',
        number_of_repetitions = number_of_repetitions,
        collect = collect,
        ...)
    return(out)
}

.bootstrap<-setClass(
    "bootstrap",
    contains=c('resampler'),
    slots=c(
        number_of_repetitions='entity',
        collect='entity',
        results='data.frame',
        metric='data.frame',
        collected='entity'
    ),
    prototype = list(
        name='Bootstrap resampling',
        description=paste0('In bootstrap resampling a subset of samples is ',
        'selected at random with replacement to form a training set. Any ',
            'sample not selected for training is included in the test set. ',
            'This process is repeated many times, and performance metrics are ',
            'computed for each repetition.'),
        type='permutation',
        result='results',
        ontology='STATO:0000548',
        .params=c('number_of_repetitions','collect'),
        .outputs=c('results','metric','collected'),

        number_of_repetitions=entity(name = 'Number of iterations',
            description='The number of bootstrap repetitions.',
            value = 10,
            max_length = 1,
            type=c('numeric','integer')),
        collect=entity(name = 'Collect output',
            description=paste0('The name of a model output to collect over all ',
            'bootstrap repetitions, in addition to the input metric.'),
            value = 'vip',
            max_length = 1,
            type=c('character')),
        collected=entity(name='collected output',
            type=c('logical','list'),
            value=NA,max_length=Inf)
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("bootstrap",'DatasetExperiment','metric'),
    definition=function(I,D,MET=NULL)
    {

        X=D$data
        y=D$sample_meta
        # get the WF
        WF=models(I)
        n=param_value(I,'number_of_iterations')

        all_results=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)

        collected=list()

        for (i in 1:n)
        {
            results=data.frame('actual'=y[,1],'predicted'=y[,1],'permutation'=i)

            # generate a random sample order
            order=sample.int(nrow(X),replace = TRUE) # WITH REPLACEMENT

            # permute
            Xp=X[order,,drop=FALSE]
            Yp=as.data.frame(y[order,,drop=FALSE])

            # rebuild datasets
            Dp=DatasetExperiment(data=Xp,sample_meta=Yp,variable_meta=D$variable_meta)

            if (is(WF,'model_OR_model_seq'))
            {
                ## permuted labels
                # train
                WF=model_train(WF,Dp)
                # predict
                WF=model_predict(WF,Dp)
                p=predicted(WF)
                results[,2]=p[,1]
                all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=results

                if (!is.na(I$collect)) {
                    if (is(WF,'model')) {
                        collected=c(collected,list(output_value(WF,I$collect)))
                    } else {
                        # if sequence assume collecting from last index
                        collected=c(collected,list(output_value(WF[length(WF)],I$collect)))
                    }
                    I$collected=collected
                }


            }

            if (is(WF,'iterator'))
            {

                stop('not implemented yet')
            }

        }
        # store results
        output_value(I,'results')=all_results
        return(I)
    }
)


