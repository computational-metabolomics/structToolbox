#' grid_search_1d class
#'
#' carries out a grid search for a single parameter
#' @export grid_search_1d
#' @examples
#' M = grid_search_1d()
grid_search_1d<-setClass(
    "grid_search_1d",
    contains='resampler',
    slots=c(params.param_to_optimise='character',
        params.search_values='numeric',
        params.model_index='numeric',
        params.factor_name='character',
        params.max_min='character',
        outputs.results='data.frame',
        outputs.metric='data.frame',
        outputs.optimum_value='numeric'

    ),
    prototype = list(name='1D grid search',
        type="optimisation",
        result='results'
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("grid_search_1d",'dataset','metric'),
    definition=function(I,D,MET)
    {
        fn=I$factor_name
        X=dataset.data(D)
        WF=models(I)
        sv=param.value(I,'search_values')
        n=length(sv)
        idx=param.value(I,'model_index')
        name=param.value(I,'param_to_optimise')
        all_results=data.frame('actual'=rep(dataset.sample_meta(D)[,fn],n),'predicted'=rep(D$sample_meta[,fn],n),'search.value'=0)
        for (i in 1:n)
        {
            if (is(WF,'model_OR_model.seq'))
            {
                # for each value, set it as the chosen parameter, then train the workflow
                #results for this  parameter value
                perm_results=data.frame('actual'=dataset.sample_meta(D)[,fn],'predicted'=dataset.sample_meta(D)[,fn],'search_value'=sv[i])
                # set the parameter value
                param.value(WF[idx],name)=sv[i]
                # train the model
                WF=model.train(WF,D)
                # apply the model
                WF=model.predict(WF,D)
                p=predicted(WF) # get the prediction output and collect
                perm_results[,2]=p[,1]
                all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=perm_results # collate results
            }  else { # must be an iterator
                param.value(WF[idx],name)=sv[i]
                WF=run(WF,D,MET)
                v=output.value(WF,'metric')
                if (i==1)
                {
                    all_results=v
                } else
                {
                    all_results=rbind(all_results,v)
                }
            }
        }
        models(I)=WF
        output.value(I,'results')=all_results

        results=output.value(I,'results')

        if (is(models(I),'model_OR_model.seq'))
        { # if a model or list then apply the metric

            k=length(unique((results$search.value)))
            ts.metric=numeric(k)
            for (i in 1:k)
            {
                ts=results[results$search.value==i,]
                MET=calculate(MET,ts$actual,ts$predicted)
                ts.metric[i]=value(MET)
            }

            # index of minimum
            if (I$max_min=='min') {
                idx=first_min(ts.metric)
            } else if (I$max_min=='max') {
                idx=first_min(-ts.metric)
            } else {
                stop('not a valid max_min choice')
            }

            out=data.frame('metric'=class(MET),'value'=ts.metric,'search.value'=param.value(I,'search_values'))
            output.value(I,'metric')=out
            output.value(I,'optimum_value')=out$search.value[idx]

        } else {
            # if not a model or list then the metric has already been applied, we just need to choose the optimum
            if (I$max_min=='min') {
                idx=first_min(results$mean)
            } else if (I$max_min=='max') {
                idx=first_min(-results$mean)
            } else {
                stop('not a valid max_min choice')
            }
            out=data.frame('metric'=class(MET),'value'=results$mean[idx],'search.value'=param.value(I,'search_values')[idx])
            output.value(I,'metric')=out
            output.value(I,'optimum_value')=out$search.value

        }
        return(I)
    }
)


#' grid_search_plot
#'
#' plots the result of the evaluated models for against the values of the optimisation paramter within the search range.
#'
#' @import struct
#' @export gs_line
#' @examples
#' C = gs_line()
gs_line<-setClass(
    "gs_line",
    contains='chart',
    prototype = list(name='Grid search line plot',
        description='Plots the result of the optimisation',
        type="line"
    )
)

#' @export
#' @inherit struct::chart.plot
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("gs_line",'grid_search_1d'),
    definition=function(obj,dobj)
    {
        A=result(dobj)
        opt=output.value(dobj,'optimum_value')
        A$values=param.value(dobj,'search_values')
        out=ggplot(data=A, aes_(x=~values,y=~mean,group=~1)) +
            geom_errorbar(aes_(ymin=~mean-(1.96*`sd`), ymax=~mean+(1.96*`sd`)), width=.1) +
            geom_line(color="red")+
            geom_point() +
            geom_point(data=A[A$values==as.numeric(opt),],aes_(x=~values,y=~mean),group=1,color='blue',shape=1,size=4) +
            ggtitle(NULL, subtitle=paste0('Suggested optimum: ',opt)) +
            theme_Publication(base_size = 12) +
            xlab(param.name(dobj,'param_to_optimise')) +
            ylab(A$metric[1])
        return(out)
    }
)


first_min=function(x,t=0.02)
{

    idxs=1 # force first index to be the first value
    # search for v
    for (i in 2:(length(x)-1))
    {
        if ((x[i-1]>x[i]) & (x[i]<=x[i+1]))
        {
            # add minimum to list
            idxs=c(idxs,i)
        }
    }
    if (length(idxs)==1)
    { # only 1 minima found, so return it
        return(idxs[1])
    }
    # scan over minima
    id=1
    lo=x[idxs[1]]
    for (i in 2:length(idxs))
    {
        if (x[idxs[i]]<((1-t)*lo)) # update min if it exceeds threshold for improvement
        {
            lo=x[idxs[i]]
            id=i
        }
    }
    return(idxs[id])

}
