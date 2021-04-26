#' @eval get_description('grid_search_1d')
#' @export grid_search_1d
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' # some preprocessing
#' M = pqn_norm(qc_label='QC',factor_name='Class') +
#'     knn_impute() +
#'     glog_transform(qc_label='QC',factor_name='Class') +
#'     filter_smeta(factor_name='Class',levels='QC',mode='exclude')
#' M=model_apply(M,D)
#' D=predicted(M)
#'
#' # reduce number of features for this example
#' D=D[,1:10]
#'
#' # optmise number of components for PLS model
#' I = grid_search_1d(param_to_optimise='number_components',search_values=1:5,
#'         model_index=2,factor_name='Class') *
#'         (mean_centre()+PLSDA(factor_name='Class'))
#' I = run(I,D,balanced_accuracy())
#'
grid_search_1d = function(param_to_optimise,search_values,model_index,factor_name,max_min='min',...) {
    out=struct::new_struct('grid_search_1d',
        param_to_optimise=param_to_optimise,
        search_values=search_values,
        model_index=model_index,
        factor_name=factor_name,
        max_min=max_min,
        ...)
    return(out)
}


.grid_search_1d<-setClass(
    "grid_search_1d",
    contains='resampler',
    slots=c(
        param_to_optimise='entity',
        search_values='entity',
        model_index='entity',
        factor_name='entity',
        max_min='entity',
        results='data.frame',
        metric='data.frame',
        optimum_value='numeric'
        
    ),
    prototype = list(name='1D grid search',
        type="optimisation",
        result='results',
        .params=c('param_to_optimise','search_values','model_index','factor_name','max_min'),
        .outputs=c('results','metric','optimum_value'),
        name = 'One dimensional grid search',
        description = paste0('A one dimensional grid search calculates a ',
            'performance metric for a model at evenly spaced values for a model ',
            'input parameter. The "optimum" value for the parameter is suggested as the one ',
            'which maximises performance, or minimises error (whichever is ',
            'appropriate for the chosen metric)'
        ),
        param_to_optimise = entity(
            name = 'Paramater to optimise',
            description = paste0('The name of the model input parameter that is the ',
            'focus of the search.'),
            value=character(0),
            type='character',
            max_length=1
        ),
        search_values = entity(
            name = 'Search values',
            description = paste0('The values of the input parameter being optimised.'),
            value=numeric(0),
            type='ANY'
        ),
        model_index = entity(
            name = 'Model index',
            description = paste0('The index of the model in the sequence ',
            'that uses the parameter being optimised.'),
            value=numeric(0),
            type=c('numeric','integer'),
            max_length=1
        ),
        factor_name=ents$factor_name,
        max_min = enum(
            name='Maximise or minimise',
            description=c(
                'max' = paste0('The optimium parameter value is suggested ',
                    'based on maximising the performance metric'),
                'min' = paste0('The optimium parameter value is suggested ',
                    'based on minimising the performance metric')
            ),
            type='character',
            value='min',
            allowed=c('max','min'),
            max_length=1
        )
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("grid_search_1d",'DatasetExperiment','metric'),
    definition=function(I,D,MET)
    {
        fn=I$factor_name
        X=D$data
        WF=models(I)
        sv=param_value(I,'search_values')
        n=length(sv)
        idx=param_value(I,'model_index')
        name=param_value(I,'param_to_optimise')
        all_results=data.frame('actual'=rep(D$sample_meta[,fn],n),'predicted'=rep(D$sample_meta[,fn],n),'search.value'=0)
        for (i in 1:n)
        {
            if (is(WF,'model_OR_model_seq'))
            {
                if (is(WF,'model')) {
                    # convert to model sequence
                    WF = model_seq()+WF
                }
                # for each value, set it as the chosen parameter, then train the workflow
                #results for this  parameter value
                perm_results=data.frame('actual'=D$sample_meta[,fn],'predicted'=D$sample_meta[,fn],'search_value'=sv[i])
                # set the parameter value
                param_value(WF[idx],name)=sv[i]
                # train the model
                WF=model_train(WF,D)
                # apply the model
                WF=model_predict(WF,D)
                p=predicted(WF) # get the prediction output and collect
                perm_results[,2]=p[,1]
                all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=perm_results # collate results
            }  else { # must be an iterator
                param_value(WF[idx],name)=sv[i]
                WF=run(WF,D,MET)
                v=output_value(WF,'metric')
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
        output_value(I,'results')=all_results
        
        results=output_value(I,'results')
        
        if (is(models(I),'model_OR_model_seq'))
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
            
            out=data.frame('metric'=class(MET)[1],'value'=ts.metric,'search.value'=param_value(I,'search_values'))
            output_value(I,'metric')=out
            output_value(I,'optimum_value')=out$search.value[idx]
            
        } else {
            # if not a model or list then the metric has already been applied, we just need to choose the optimum
            if (I$max_min=='min') {
                idx=first_min(results$mean)
            } else if (I$max_min=='max') {
                idx=first_min(-results$mean)
            } else {
                stop('not a valid max_min choice')
            }
            out=data.frame('metric'=class(MET)[1],'value'=results$mean[idx],'search.value'=param_value(I,'search_values')[idx])
            output_value(I,'metric')=out
            output_value(I,'optimum_value')=out$search.value
            
        }
        return(I)
    }
)


#' @eval get_description('gs_line')
#' @export gs_line
#' @examples
#' C = gs_line()
gs_line = function(...) {
    out=struct::new_struct('gs_line',...)
    return(out)
}


.gs_line<-setClass(
    "gs_line",
    contains='chart',
    prototype = list(
        name='Grid search line plot',
        description=paste0('A plot of the calculated performance metric against ',
        'the model input parameter values used to train the model. The ',
        'optimum parameter value is indicated based on minimising ',
        '(or maximising) the chosen metric.'),
        type="line"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("gs_line",'grid_search_1d'),
    definition=function(obj,dobj)
    {
        A=result(dobj)
        opt=output_value(dobj,'optimum_value')
        A$values=param_value(dobj,'search_values')
        out=ggplot(data=A, aes_(x=~values,y=~mean,group=~1)) +
            geom_errorbar(aes_(ymin=~mean-(1.96*`sd`), ymax=~mean+(1.96*`sd`)), width=.1) +
            geom_line(color="red")+
            geom_point() +
            geom_point(data=A[A$values==as.numeric(opt),],aes_(x=~values,y=~mean),group=1,color='blue',shape=1,size=4) +
            ggtitle(NULL, subtitle=paste0('Suggested optimum: ',opt)) +
            theme_Publication(base_size = 12) +
            xlab(param_name(dobj,'param_to_optimise')) +
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
