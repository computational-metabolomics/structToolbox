#' permutation test class
#'
#' Applies a permutation test to a model or model_seq()
#' @examples
#' I=permutation_test()
#'
#' @param ... slots and values for the new object 
#' @export permutation_test
permutation_test = function(...) {
    out=.permutation_test()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.permutation_test<-setClass(
    "permutation_test",
    contains='resampler',
    slots=c(params_number_of_permutations='numeric',
        outputs_results.permuted='data.frame',
        outputs_results.unpermuted='data.frame',
        outputs_metric='data.frame'
    ),
    prototype = list(name='permutation test',
        type='permutation',
        result='results',
        params_number_of_permutations=10
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template run
setMethod(f="run",
    signature=c("permutation_test",'DatasetExperiment','metric'),
    definition=function(I,D,MET=NULL)
    {

        X=D$data
        y=D$sample_meta
        # get the WF
        WF=models(I)
        n=param_value(I,'number_of_permutations')

        all_results_permuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)
        all_results_unpermuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)

        for (i in 1:n)
        {
            perm_results=data.frame('actual'=y[,1],'predicted'=y[,1],'permutation'=i)
            unperm_results=data.frame('actual'=y[,1],'predicted'=y[,1],'permutation'=i)
            # generate a random sample order
            order_x=sample.int(nrow(X))
            order_y=sample.int(nrow(X))
            order_y2=order_x # the same order for y

            # permute
            Xp=X[order_x,,drop=FALSE]
            Yp=as.data.frame(y[order_y,,drop=FALSE])
            Yp2=as.data.frame(y[order_y2,,drop=FALSE])

            # rebuild datasets
            Dp=D
            Dp$data=Xp
            Dp$sample_meta=Yp

            D2=D
            D2$data=Xp
            D2$sample_meta=Yp2

            if (is(WF,'model_OR_model_seq'))
            {
                ## permuted labels
                # train
                WF=model_train(WF,Dp)
                # predict
                WF=model_predict(WF,Dp)
                p=predicted(WF)
                perm_results[,2]=p[,1]
                all_results_permuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=perm_results

                ## real labels
                # train
                WF=model_train(WF,D2)
                # predict
                WF=model_predict(WF,D2)
                p=predicted(WF)
                unperm_results[,2]=p[,1]
                all_results_unpermuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=unperm_results
            }

            if (is(WF,'iterator'))
            {

                ## permuted
                WF=run(WF,Dp,MET)
                v=output_value(WF,'metric')

                if (i==1)
                {
                    all_results_permuted=v
                }
                else
                {
                    all_results_permuted=rbind(all_results_permuted,v)
                }


                ## real
                WF=run(WF,D2,MET)
                w=output_value(WF,'metric')
                if (i==1)
                {
                    all_results_unpermuted=w
                }
                else
                {
                    all_results_unpermuted=rbind(all_results_unpermuted,w)
                }

            }

        }
        # store results
        output_value(I,'results.permuted')=all_results_permuted
        output_value(I,'results.unpermuted')=all_results_unpermuted
        return(I)
    }
)


#' permutation_test.boxplot class
#'
#' plots the results of a permutation test as a boxplot
#' @examples
#' C = permutation_test.boxplot()
#' @param ... slots and values for the new object 
#' @export permutation_test.boxplot
permutation_test.boxplot<-setClass(
    "permutation_test.boxplot",
    contains='chart',
    prototype = list(name='permutation test',
        type='boxplot',
        description='a boxplot of the calculated metric for the model with permuted and unpermuted data'
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c('permutation_test.boxplot','permutation_test'),
    definition=function(obj,dobj)
    {
        p=output_value(dobj,'results.permuted')
        u=output_value(dobj,'results.unpermuted')

        A=data.frame('value'=c(p$mean,u$mean),'DatasetExperiment'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
        plotClass= createClassAndColors(A$DatasetExperiment)
        out=ggplot(data=A,aes_(x=~DatasetExperiment,y=~value,color=~DatasetExperiment)) +
            geom_boxplot()+
            theme_Publication(base_size = 12)+
            scale_color_manual(values=plotClass$manual_colors,name='DatasetExperiment') +
            theme(legend.position="none")
        return(out)
    }
)

#' permutation_test.violin class
#'
#' plots the results of a permutation test as a boxplot
#' @examples
#' C = permutation_test.violin()
#' @param ... slots and values for the new object 
#' @export permutation_test.violin
permutation_test.violin<-setClass(
    "permutation_test.violin",
    contains='chart',
    prototype = list(name='permutation test',
        type='violin',
        description='a violin plot of the calculated metric for the model with permuted and unpermuted data'
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c('permutation_test.violin','permutation_test'),
    definition=function(obj,dobj)
    {
        p=output_value(dobj,'results.permuted')
        u=output_value(dobj,'results.unpermuted')

        A=data.frame('value'=c(p$mean,u$mean),'DatasetExperiment'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
        plotClass= createClassAndColors(A$DatasetExperiment)
        out=ggplot(data=A,aes_(x=~DatasetExperiment,y=~value,color=~DatasetExperiment)) +
            geom_violin(trim=F)+
            theme_Publication(base_size = 12)+
            scale_color_manual(values=plotClass$manual_colors,name='DatasetExperiment') +
            theme(legend.position="none")
        return(out)
    }
)

#' permutation_test_hist class
#'
#' plots the results of a permutation test as histograms
#' @examples
#' C = permutation_test_hist()
#' @param ... slots and values for the new object 
#' @export permutation_test_hist
permutation_test_hist<-setClass(
    "permutation_test_hist",
    contains='chart',
    prototype = list(name='permutation test',
        type='histogram',
        description='a histogram plot of the calculated metric for the model with permuted and unpermuted data'
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c('permutation_test_hist','permutation_test'),
    definition=function(obj,dobj)
    {
        p=output_value(dobj,'results.permuted')
        u=output_value(dobj,'results.unpermuted')

        A=data.frame('value'=c(p$mean,u$mean),'DatasetExperiment'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
        plotClass= createClassAndColors(A$DatasetExperiment)
        out=ggplot(data=A,aes_(x=~value,color=~DatasetExperiment)) +
            geom_freqpoly(binwidth = 0.05)+
            theme_Publication(base_size = 12)+
            scale_color_manual(values=plotClass$manual_colors,name='DatasetExperiment')
        return(out)
    }
)

#' permutation_test.scatter class
#'
#' plots the results of a permutation test as histograms
#' @examples
#' C = permutation_test.scatter()
#' @param ... slots and values for the new object 
#' @export permutation_test.scatter
permutation_test.scatter<-setClass(
    "permutation_test.scatter",
    contains='chart',
    prototype = list(name='permutation test',
        type='scatter',
        description='a scatter plot of the calculated metric for the model with permuted and unpermuted data'
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c('permutation_test.scatter','permutation_test'),
    definition=function(obj,dobj)
    {
        p=output_value(dobj,'results.permuted')
        u=output_value(dobj,'results.unpermuted')

        A=data.frame('value'=c(p$mean,u$mean),'DatasetExperiment'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
        plotClass= createClassAndColors(A$DatasetExperiment)
        out=ggplot(data=A,aes_(x=1:nrow(A),y=~value,color=~DatasetExperiment)) +
            geom_point(na.rm=T)+
            theme_Publication(base_size = 12)+
            scale_color_manual(values=plotClass$manual_colors,name='DatasetExperiment') +
            theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
        return(out)
    }
)
