#' Bootstrap class
#'
#' Applies bootstrapping to a model or model_seq(). Any output from the model can
#' be 'collected' over all bootstrap repetitions for further analysis.
#'
#' @examples
#' D = iris_DatasetExperiment()
#' I = bootstrap(number_of_iterations = 5, collect='vip') *
#'     (mean_centre() + PLSDA(factor_name = 'Species'))
#' I = run(I,D,balanced_accuracy())
#'
#' @param number_of_iterations The number of bootstrap iterations to run
#' @param collect The name of model output to collect over all iterations
#' @param ... additional slots and values passed to struct_class
#' @return A struct iterator object
#' @export bootstrap
bootstrap = function(number_of_iterations = 100, collect, ...) {
    out=struct::new_struct('bootstrap',
        number_of_iterations = number_of_iterations,
        collect = collect,
        ...)
    return(out)
}

.bootstrap<-setClass(
    "bootstrap",
    contains='resampler',
    slots=c(
        number_of_iterations='numeric',
        collect='character',
        results='data.frame',
        metric='data.frame',
        collected='entity'
    ),
    prototype = list(name='permutation test',
        type='permutation',
        result='results',
        .params=c('number_of_iterations','collect'),
        .outputs=c('results','metric','collected'),

        number_of_iterations=10,
        collect='vip',
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


