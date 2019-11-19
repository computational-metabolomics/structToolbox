#' forward selection by rank
#'
#' Forward selection by rank is a stepwise procedures that includes features
#' incrementally based on their rank. Any measure for ranking the features may
#' be used e.g. PLS VIP score, ttest p-value etc.
#'
#' @param min_no_vars minimum number of features to test
#' @param max_no_vars maximum numbe ro features to test
#' @param step_size the size of the incremenent between min and max no of vars
#' @param factor_name the sample-meta colum to use
#' @param variable_rank a vector of values that can be used to rank the features,
#' where the smallest value is the first rank.
#'
#' @examples
#' # some data
#' D = sbcms_dataset(filtered=TRUE)
#'
#' # normalise, impute and scale then remove QCs
#' P = pqn_norm(qc_label='QC',factor_name='class') +
#'     knn_impute(neighbours=5) +
#'     glog_transform(qc_label='QC',factor_name='class') +
#'     filter_smeta(mode='exclude',levels='QC',factor_name='class')
#' P = model.apply(P,D)
#' D = predicted(P)
#'
#' # forward selection using a PLSDA model
#' M = forward_selection_byrank(factor_name='class',
#'                              min_no_vars=2,
#'                              max_no_vars=11,
#'                              variable_rank=1:2063) *
#'     (mean_centre() + PLSDA(number_components=1,
#'                            factor_name='class'))
#' M = run(M,D,balanced_accuracy())
#'
#' @export forward_selection_byrank
forward_selection_byrank <- setClass(
    # name of class
    "forward_selection_byrank",
    # define slots
    slots=c(params.variable_rank="numeric",
        params.min_no_vars="numeric",
        params.max_no_vars="numeric",
        params.step_size="numeric",
        params.factor_name='character',
        outputs.metric="data.frame",
        outputs.results='data.frame',
        outputs.chosen_vars='numeric',
        outputs.smoothed='numeric',
        outputs.searchlist='numeric'
    ),
    prototype = list(params.variable_rank=c(1,2,3),
        params.min_no_vars=1,
        params.max_no_vars=100,
        params.step_size=1,
        result='results'
    ),
    contains = 'resampler'
)


#' @export
#' @template run
setMethod(f="run",
    signature=c("forward_selection_byrank",'dataset','metric'),
    definition=function(I,D,MET)
    {
        X=dataset.data(D)
        vr=param.value(I,'variable_rank')
        # rank the variables
        R = rank(vr)
        O = 1:length(vr)

        WF=models(I)

        min_vars=param.value(I,'min_no_vars')
        max_vars=min(c(ncol(X),param.value(I,'max_no_vars')))
        step_size=param.value(I,'step_size')

        searchlist=seq(min_vars,max_vars,by=step_size)
        n=length(searchlist)

        all_results=data.frame('actual'=rep(dataset.sample_meta(D)[,I$factor_name],n),'predicted'=rep(dataset.sample_meta(D)[,I$factor_name],n),'no_features'=0)

        counter=1
        for (i in searchlist)
        {
            # reduce to include only the desired number of variables
            Xi=X[,R<=i,drop=FALSE]
            Di=D
            dataset.data(Di)=Xi
            if (is(WF,'model_OR_model.seq')) {
                perm_results=data.frame('actual'=dataset.sample_meta(D)[,I$factor_name],'predicted'=dataset.sample_meta(D)[,I$factor_name],'no_features'=i)
                # train the workflow
                WF=model.train(WF,Di)
                # apply the workflow
                WF=model.predict(WF,Di)
                p=predicted(WF) # get the prediction output and collect
                perm_results[,2]=p[,1]
                all_results[((nrow(X)*(counter-1))+1):(nrow(X)*counter),]=perm_results # collate results
            }  else
            { # must be an iterator

                WF=run(WF,Di,MET)
                v=output.value(WF,'metric')
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
        output.value(I,'results')=all_results
        output.value(I,'searchlist')=searchlist
        # evaluate using the metric
        #I=evaluate(I,MET)

        results=output.value(I,'results')
        searchlist=output.value(I,'searchlist')

        if (is(models(I),'model_OR_model.seq'))
        { # if a model or list then apply the metric

            k=length(searchlist)
            ts.metric=numeric(k)
            for (i in 1:k)
            {
                ts=results[results$no_features==searchlist[i],]
                MET=calculate(MET,ts$actual,ts$predicted)
                ts.metric[i]=value(MET)
            }

            value=ts.metric
            df=data.frame(metric=name(MET),mean=value,sd=NA)
        } else
        {
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
        output.value(I,'chosen_vars')=O[R<=lo_var_ind]
        output.value(I,'smoothed')=smoothed25
        output.value(I,'metric')=df

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
    for (i in 1:k)
    {
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
        smoothed=stats::predict(loessMod,newdata=xx2)
        residual[i]=sum((smoothed-yy2)^2)
    }
    return(sqrt(mean(residual)))
}

#' forward_selection_plot
#'
#' Plots the result of the evaluated models against the values the number of
#' features within the search range for forward_selection_by_rank objects.
#'
#' @import struct
#' @export fs_line
#' @examples
#' # some data
#' D = sbcms_dataset(filtered=TRUE)
#'
#' # normalise, impute and scale then remove QCs
#' P = pqn_norm(qc_label='QC',factor_name='class') +
#'     knn_impute(neighbours=5) +
#'     glog_transform(qc_label='QC',factor_name='class') +
#'     filter_smeta(mode='exclude',levels='QC',factor_name='class')
#' P = model.apply(P,D)
#' D = predicted(P)
#'
#' # forward selection using a PLSDA model
#' M = forward_selection_byrank(factor_name='class',
#'                              min_no_vars=2,
#'                              max_no_vars=11,
#'                              variable_rank=1:2063) *
#'     (mean_centre() + PLSDA(number_components=1,
#'                            factor_name='class'))
#' M = run(M,D,balanced_accuracy())
#'
#' # chart
#' C = fs_line()
#' chart.plot(C,M)
#'
fs_line<-setClass(
    "fs_line",
    contains='chart',
    prototype = list(name='Forward selection line plot',
        description='Plots the result of forward selection',
        type="line"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("fs_line",'forward_selection_byrank'),
    definition=function(obj,dobj)
    {
        A=dobj$metric
        A$smoothed=output.value(dobj,'smoothed')
        opt=length(output.value(dobj,'chosen_vars'))
        A$values=output.value(dobj,'searchlist')
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
