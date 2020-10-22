#' @eval get_description('permute_sample_order')
#' @examples
#' C = permute_sample_order()
#' @export permute_sample_order
permute_sample_order = function(number_of_permutations=10,...) {
    out=struct::new_struct('permute_sample_order',
        number_of_permutations=number_of_permutations,
        ...)
    return(out)
}

.permute_sample_order<-setClass(
    "permute_sample_order",
    contains='resampler',
    slots=c(number_of_permutations='entity',
        results='data.frame',
        metric='data.frame',
        metric.train='numeric'
    ),
    prototype = list(name='Permute Sample Order',
        type="permutation",
        result='results',
        number_of_permutations=entity(
            name='Number of permutations',
            description = 'The number of times the sample order is permuted.',
            type=c('numeric','integer'),
            value=100,
            max_length=1
        ),
        .params=c('number_of_permutations'),
        .outputs=c('results','metric','metric.train'),
        description=paste0('The order of samples in the data matrix is ',
        'randomly permuted. The relationship between the samples and the ',
        'sample meta data is maintained.')
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("permute_sample_order",'DatasetExperiment','metric'),
    definition=function(I,D,MET) {

        # get the data
        X=D$data
        y=D$sample_meta

        # get the WF
        WF=models(I)
        n=param_value(I,'number_of_permutations')
        result=data.frame(nrow=nrow(X),ncol=n)
        all_results=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)
        actual=result # for storing true y values over all permutations
        value=numeric()
        Dperm=D
        for (i in 1:n)
        {
            # generate a random sample order
            order_x=sample(nrow(X))
            order_y=order_x

            # permute
            Xp=X[order_x,,drop=FALSE]
            Yp=y[order_y,,drop=FALSE]

            # rebuild DatasetExperiment object
            Dperm=DatasetExperiment(data=Xp,sample_meta=Yp,variable_meta=D$variable_meta)

            # WF can be a model/model list
            if (is(WF,'model_OR_model_seq'))
            {
                perm_results=data.frame('actual'=Yp[,1],'predicted'=Yp[,1],'permutation'=i)
                WF=model_train(WF,Dperm)
                WF=model_predict(WF,Dperm)
                p=predicted(WF) # get the prediction output and collect over all permutations
                perm_results[,2]=p[,1]
                all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=perm_results
            }
            else
            {
                WF=run(WF,Dperm,MET)
                v=output_value(WF,'metric')
                if (i==1)
                {
                    all_results=v
                }
                else
                {
                    all_results=rbind(all_results,v)
                }
            }

        }
        # store results
        models(I)=WF
        output_value(I,'results')=all_results
        I=evaluate(I,MET)
        return(I)
    }
)

setMethod(f="evaluate",
    signature=c("permute_sample_order","metric"),
    definition=function(I,MET)
    {
        results=output_value(I,'results')

        if (is(models(I),'model_OR_model_seq'))
        { # if a model or list then apply the metric

            k=max(results$permutation)
            ts.metric=numeric(k)
            for (i in 1:k)
            {
                ts=results[results$permutation==i,]
                MET=calculate(MET,ts$actual,ts$predicted)
                ts.metric[i]=value(MET)
            }
            out=data.frame('metric'=class(MET)[1],'mean'=mean(ts.metric),'sd'=sd(ts.metric))


            output_value(I,'metric')=out
            output_value(I,'metric.train')=ts.metric
            return(I)
        } else {
            # if not a model or list then the metric has already been applied, we just need to average
            out=results[1,,drop=FALSE]
            out$mean=mean(results$mean)
            out$sd=sd(results$mean)
            output_value(I,'metric')=out
            return(I)
        }
    }
)



