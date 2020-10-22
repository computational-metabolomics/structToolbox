#' @eval get_description('permutation_test')
#' @examples
#' I=permutation_test(factor_name='Species')
#' @export permutation_test
permutation_test = function(number_of_permutations=50,factor_name,...) {
    out=struct::new_struct('permutation_test',
        number_of_permutations=number_of_permutations,
        factor_name=factor_name,
        ...)
    return(out)
}


.permutation_test<-setClass(
    "permutation_test",
    contains='resampler',
    slots=c(
        number_of_permutations='entity',
        results.permuted='data.frame',
        results.unpermuted='data.frame',
        metric='data.frame',
        factor_name='entity'
    ),
    prototype = list(name='Permutation test',
        type='permutation',
        result='results.permuted',
        description=paste0('A permutation test generates a "null" model by ',
        'randomising the response (for regression models) or group labels ',
        '(for classification models). This is repeated many times to generate ',
        'a distribution of performance metrics for the null model. This ',
        'distribution can then be compared to the performance of the true ',
        'model. If there is overlap between the true and null model ',
        'performances then the model is overfitted.'),
        number_of_permutations=10,
        .params=c('number_of_permutations','factor_name'),
        .outputs=c('results.permuted','results.unpermuted','metric'),
        factor_name=ents$factor_name,
        number_of_permutations=entity(
            name = 'Number of permutations',
            description = 'The number of permutations.',
            type=c('numeric','integer'),
            value=100,
            max_length=1
        )
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("permutation_test",'DatasetExperiment','metric'),
    definition=function(I,D,MET=NULL)
    {
        
        X=D$data
        y=D$sample_meta[,I$factor_name,drop=FALSE]
        # get the WF
        WF=models(I)
        n=param_value(I,'number_of_permutations')
        
        all_results_permuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)
        all_results_unpermuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)
        
        
        pe.metric=numeric(n)
        un.metric=numeric(n)
        for (i in 1:n) {
            
            # generate a random sample order
            order_x=sample.int(nrow(X))
            order_y=sample.int(nrow(X))
            order_y2=order_x # the same order for y
            
            # permute
            Xp=X[order_x,,drop=FALSE]
            Yp=as.data.frame(y[order_y,,drop=FALSE])
            Yp2=as.data.frame(y[order_y2,,drop=FALSE])
            rownames(Yp)=rownames(Xp) # force same rownames even though no longer a genuine match
            
            # rebuild datasets
            Dp=DatasetExperiment(data=Xp,sample_meta=Yp,variable_meta=D$variable_meta)
            D2=DatasetExperiment(data=Xp,sample_meta=Yp2,variable_meta=D$variable_meta)
            
            perm_results=data.frame('actual'=Yp[,1],'predicted'=Yp[,1],'permutation'=i)
            unperm_results=data.frame('actual'=Yp2[,1],'predicted'=Yp2[,1],'permutation'=i)
            
            if (is(WF,'model_OR_model_seq')) {
                ## permuted labels
                # train
                WF=model_train(WF,Dp)
                # predict
                WF=model_predict(WF,Dp)
                p=predicted(WF)
                perm_results[,2]=p[,1]
                all_results_permuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=perm_results
                
                # calculate metric
                MET=calculate(MET,perm_results$actual,perm_results$predicted)
                pe.metric[i]=value(MET)
                
                ## real labels
                # train
                WF=model_train(WF,D2)
                # predict
                WF=model_predict(WF,D2)
                p=predicted(WF)
                unperm_results[,2]=p[,1]
                all_results_unpermuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=unperm_results
                
                # calculate metric
                MET=calculate(MET,unperm_results$actual,unperm_results$predicted)
                un.metric[i]=value(MET)
            }
            
            if (is(WF,'iterator')) {
                
                ## permuted
                WF=run(WF,Dp,MET)
                v=output_value(WF,'metric')
                
                if (i==1) {
                    all_results_permuted=v
                }
                else {
                    all_results_permuted=rbind(all_results_permuted,v)
                }
                
                ## real
                WF=run(WF,D2,MET)
                w=output_value(WF,'metric')
                if (i==1) {
                    all_results_unpermuted=w
                }
                else {
                    all_results_unpermuted=rbind(all_results_unpermuted,w)
                }
                
            }
            
        }
        # store results
        output_value(I,'results.permuted')=all_results_permuted
        output_value(I,'results.unpermuted')=all_results_unpermuted
        I$metric=data.frame('metric'=class(MET)[1],'permuted'=pe.metric,'unpermuted'=un.metric)
        return(I)
    }
)


#' permutation_test_plot class
#'
#' Plots the results of a permutation test.
#' @examples
#' C = permutation_test_plot(style='boxplot')
#' @param style The plot style. One of 'boxplot', 'violin', 'histogram', 'density' or 'scatter'.
#' @param binwidth Binwidth for the "histogram" style. Ignored for all other styles.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export permutation_test_plot
permutation_test_plot = function(style = 'boxplot',binwidth=0.05,...) {
    out=struct::new_struct('permutation_test_plot',
        style=style,
        binwidth=binwidth,
        ...)
    return(out)
}


.permutation_test_plot<-setClass(
    "permutation_test_plot",
    contains='chart',
    slots=c(style='enum',
        binwidth='numeric'),
    prototype = list(
        name='Permutation test plot',
        type='boxplot',
        description='A plot of the calculated metric for the model with permuted and unpermuted labels.',
        .params=c('style','binwidth'),
        style=enum(name='Plot style',
            description=c(
                'boxplot' = 'A boxplot to visualise the performance metrics',
                'violin' = 'A violin plot to visualise the performance metrics',
                'histogram' = 'A histogram of the computed performance metrics',
                'density' = 'A density plot of the computed metrics',
                'scatter' = 'A scatter plot of the computed metrics'),
            type='character',
            value='boxplot',
            allowed=c('boxplot','violin','histogram','scatter','density')
        ),
        binwidth = 0.05
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c('permutation_test_plot','permutation_test'),
    definition=function(obj,dobj) {
        
        if (is(models(dobj),'iterator')) {
            p=output_value(dobj,'results.permuted')
            u=output_value(dobj,'results.unpermuted')
            
            A=data.frame(
                'value'=c(p$mean,u$mean),
                'dataset'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
        } else {
            p=dobj$metric$permuted
            u=dobj$metric$unpermuted
            A=data.frame(
                'value'=c(p,u),
                'dataset'=c(rep('Permuted',length(p)),rep('Unpermuted',length(u))))
        }
        
        plotClass= createClassAndColors(A$dataset)
        
        if (obj$style=='boxplot') {
            out=ggplot(data=A,aes_(x=~dataset,y=~value,color=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
            geom_boxplot() +
                theme(legend.position="none") +xlab('Dataset')
            
        } else if (obj$style=='violin') {
            out=ggplot(data=A,aes_(x=~dataset,y=~value,color=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
            geom_violin(trim=F) +
                theme(legend.position="none") +xlab('Dataset')
            
        } else if (obj$style=='histogram') {
            out=ggplot(data=A,aes_(x=~value,color=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
            geom_freqpoly(binwidth = obj$binwidth)
            
        } else if (obj$style=='scatter') {
            out=ggplot(data=A,aes_(x=1:nrow(A),y=~value,color=~dataset)) +
                geom_point(na.rm=T) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
            theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
        } else if (obj$style=='density') {
            out=ggplot(data=A,aes_(x=~value,color=~dataset,fill=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
                scale_fill_manual(values=plotClass$manual_colors,name='Dataset') +
                geom_density(alpha = 0.3)
        }
        
        out = out + theme_Publication(base_size = 12) 
        
        
        return(out)
    }
)

