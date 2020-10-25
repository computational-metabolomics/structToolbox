#' @eval get_description('forward_selection_by_rank')
#' @examples
#' # some data
#' D = MTBLS79_DatasetExperiment(filtered=TRUE)
#'
#' # normalise, impute and scale then remove QCs
#' P = pqn_norm(qc_label='QC',factor_name='class') +
#'     knn_impute(neighbours=5) +
#'     glog_transform(qc_label='QC',factor_name='class') +
#'     filter_smeta(mode='exclude',levels='QC',factor_name='class')
#' P = model_apply(P,D)
#' D = predicted(P)
#'
#' # forward selection using a PLSDA model
#' M = forward_selection_by_rank(factor_name='class',
#'                              min_no_vars=2,
#'                              max_no_vars=11,
#'                              variable_rank=1:2063) *
#'     (mean_centre() + PLSDA(number_components=1,
#'                            factor_name='class'))
#' M = run(M,D,balanced_accuracy())
#'
#' @export forward_selection_by_rank
forward_selection_by_rank = function(min_no_vars=1,max_no_vars=100,step_size=1,factor_name,variable_rank,...) {
    out=struct::new_struct('forward_selection_by_rank',
        min_no_vars=min_no_vars,
        max_no_vars=max_no_vars,
        step_size=step_size,
        factor_name=factor_name,
        variable_rank=variable_rank,
        ...)
    return(out)
}


.forward_selection_by_rank <- setClass(
    # name of class
    "forward_selection_by_rank",
    # define slots
    slots=c(
        variable_rank="entity",
        min_no_vars="entity",
        max_no_vars="entity",
        step_size="entity",
        factor_name='entity',
        metric="entity",
        results='entity',
        chosen_vars='entity',
        smoothed='entity',
        searchlist='entity'
    ),
    prototype = list(variable_rank=c(1,2,3),
        name = 'Forward selection by rank',
        description = paste0('A model is trained and performance metric ',
            'computed by including increasing numbers of features in the model. ',
            'The features to be included in each step are defined by their rank, ',
            'which is computed from another variable e.g. VIP score. An "optimal"', 
            'subset of features is suggested by minimising the input performance metric.'),
        result='results',
        .params=c('min_no_vars','max_no_vars','step_size','factor_name'),
        .outputs=c('metric','results','chosen_vars','smoothed','searchlist'),
        variable_rank=entity(
            name = 'Variable rank',
            description = 'The values used to rank the features.',
            type=c('numeric','integer'),
            value=numeric(0)
        ),
        min_no_vars = entity(
            name = 'Minimum number of variables',
            description = 'The minimum number of variables to include in the model.',
            type='numeric',
            value=1,
            max_length = 1
        ),
        factor_name=ents$factor_name,
        metric=entity(
            name = 'metric',
            description = 'The value of the computed metric for each model. For nested models the metric is averaged.',
            type='data.frame',
            value=data.frame()
        ),
        results = entity(
            name  = 'Results',
            description = paste0('The predicted outputs from collated from ',
                'all models computed during forward selection.'),
            type='data.frame',
            value=data.frame()
        ),
        chosen_vars = entity(
            name = 'Chosen variables',
            description = 'The column number of the variables chosen for the best performing model.',
            type=c('numeric','integer'),
            value=numeric(0)
        ),
        smoothed = entity(
            name = 'Smoothed performance metric',
            description = 'The value of the performance metric for each evaluated model after smoothing.',
            type= 'numeric',
            value=numeric(0)
        ),
        searchlist = entity(
            name = 'Search space',
            description = 'The maxmimum rank of features included in each model.',
            type = 'numeric',
            value = numeric(0)
        ),
        max_no_vars = entity(
            name = 'Maximum number of variables',
            description = 'The maximum number of variables to include in the model.',
            type = 'numeric',
            value = 100,
            max_length = 1
        ),
        step_size = entity(
            name = 'Step size',
            description = 'The incremental change in number of features in the model.',
            value = 1,
            type = 'numeric',
            max_length = 1
        )
    ),
    contains = 'resampler'
)


#' @export
#' @template run
setMethod(f="run",
    signature=c("forward_selection_by_rank",'DatasetExperiment','metric'),
    definition=function(I,D,MET)
    {
        X=D$data
        vr=param_value(I,'variable_rank')
        # rank the variables
        R = rank(vr)
        O = 1:length(vr)
        
        WF=models(I)
        
        min_vars=param_value(I,'min_no_vars')
        max_vars=min(c(ncol(X),param_value(I,'max_no_vars')))
        step_size=param_value(I,'step_size')
        
        searchlist=seq(min_vars,max_vars,by=step_size)
        n=length(searchlist)
        
        all_results=data.frame('actual'=rep(D$sample_meta[,I$factor_name],n),'predicted'=rep(D$sample_meta[,I$factor_name],n),'no_features'=0)
        
        counter=1
        for (i in searchlist)
        {
            # reduce to include only the desired number of variables
            Di=D[,R<=i,drop=FALSE]
            if (is(WF,'model_OR_model_seq')) {
                perm_results=data.frame('actual'=D$sample_meta[,I$factor_name],'predicted'=D$sample_meta[,I$factor_name],'no_features'=i)
                # train the workflow
                WF=model_train(WF,Di)
                # apply the workflow
                WF=model_predict(WF,Di)
                p=predicted(WF) # get the prediction output and collect
                perm_results[,2]=p[,1]
                all_results[((nrow(X)*(counter-1))+1):(nrow(X)*counter),]=perm_results # collate results
            }  else {
                # must be an iterator
                WF=run(WF,Di,MET)
                v=output_value(WF,'metric')
                if (counter==1)
                {
                    all_results=v
                } else
                {
                    all_results=rbind(all_results,v)
                }
            }
            counter=counter+1
        }
        output_value(I,'results')=as.data.frame(all_results)
        output_value(I,'searchlist')=searchlist
        # evaluate using the metric
        #I=evaluate(I,MET)
        
        results=output_value(I,'results')
        searchlist=output_value(I,'searchlist')
        
        if (is(models(I),'model_OR_model_seq')) {
            # if a model or list then apply the metric
            
            k=length(searchlist)
            ts.metric=numeric(k)
            for (i in 1:k) {
                ts=results[results$no_features==searchlist[i],]
                MET=calculate(MET,ts$actual,ts$predicted)
                ts.metric[i]=value(MET)
            }
            
            value=ts.metric
            df=data.frame(metric=MET$name,mean=value,sd=NA)
        } else {
            # if not a model or list then the metric has already been applied, we just need to choose the optimum
            value=results$mean
            df=results
        }
        
        # locate optimum using a loess fit
        # cross validate to estimate span parameter
        opts=suppressWarnings(
            optimise(eval_loess,interval=c(0,1),X=searchlist,Y=value,k=50,p=0.9)
        )# cross-val smoothing param
        
        loessMod25 <- loess(value ~ searchlist, span=opts$minimum)
        smoothed25=predict(loessMod25)
        
        lo=first_min(smoothed25)
        lo_var_ind=searchlist[lo]
        output_value(I,'chosen_vars')=O[R<=lo_var_ind]
        output_value(I,'smoothed')=smoothed25
        output_value(I,'metric')=df
        
        return(I)
    }
)

eval_loess=function(x,X,Y,k=10,p=0.66)
{
    # x = span
    # X = measured values
    # Y = observed values
    # k = number of replicates
    # p = proportion in training
    
    residual=numeric(k)
    for (i in 1:k) {
        # randomly choose some data to remove
        xx=sample(X[2:length(X)-1], (length(X)-2)*p, replace = FALSE)
        xx=sort(unique(c(X[1],xx,X[length(X)]))) # keep first and last points so that loess doesnt have to extrapolate
        yy=Y[X %in% xx]
        
        xx2=X[!(X %in% xx)]
        xx2=sort(xx2)
        yy2=Y[X %in% xx2]
        
        loessMod <- loess(yy ~ xx, span=x)
        
        # check for NaN
        if (any(is.nan(loessMod$fitted))){
            residual[i]=99999
        } else {
            
            smoothed=stats::predict(loessMod,newdata=xx2)
            residual[i]=sum((smoothed-yy2)^2)
        }
    }
    return(sqrt(mean(residual)))
}

#' @eval get_description('fs_line')
#' @export fs_line
#' @examples
#' # some data
#' D = MTBLS79_DatasetExperiment(filtered=TRUE)
#'
#' # normalise, impute and scale then remove QCs
#' P = pqn_norm(qc_label='QC',factor_name='class') +
#'     knn_impute(neighbours=5) +
#'     glog_transform(qc_label='QC',factor_name='class') +
#'     filter_smeta(mode='exclude',levels='QC',factor_name='class')
#' P = model_apply(P,D)
#' D = predicted(P)
#'
#' # forward selection using a PLSDA model
#' M = forward_selection_by_rank(factor_name='class',
#'                              min_no_vars=2,
#'                              max_no_vars=11,
#'                              variable_rank=1:2063) *
#'     (mean_centre() + PLSDA(number_components=1,
#'                            factor_name='class'))
#' M = run(M,D,balanced_accuracy())
#'
#' # chart
#' C = fs_line()
#' chart_plot(C,M)
#'
fs_line = function(...) {
    out=struct::new_struct('fs_line',...)
    return(out)
}


.fs_line<-setClass(
    "fs_line",
    contains='chart',
    prototype = list(
        name='Forward selection line plot',
        description=paste0('A line plot for forward selection. The computed ',
        'model performance metric is plotted against the number of features ',
        'included in the model.'),
        type="line"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("fs_line",'forward_selection_by_rank'),
    definition=function(obj,dobj)
    {
        A=dobj$metric
        A$smoothed=output_value(dobj,'smoothed')
        opt=length(output_value(dobj,'chosen_vars'))
        A$values=output_value(dobj,'searchlist')
        out=ggplot(data=A, aes_(x=~values,y=~mean,group=~1)) +
            geom_errorbar(aes_(ymin=~mean-(1.96*`sd`), ymax=~mean+(1.96*`sd`)), width=.1) +
            geom_line(color="red",aes_(x=~values,y=~smoothed))+
            geom_point() +
            geom_point(data=A[A$values==as.numeric(opt),],aes_(x=~values,y=~smoothed),group=1,color='blue',shape=1,size=4,stroke=2) +
            ggtitle(NULL, subtitle=paste0('Suggested number of features: ',opt)) +
            set_scale() + theme_Publication(base_size = 12) +
            xlab('Number of features') +
            ylab(A$metric[1])
        
        return(out)
    }
)
