#' wilcoxon signed rank test method class
#'
#' wilcoxon signed rank test. Calculate signed rank test for all features in a
#' dataset. Non-parametric ttest.
#'
#' @import struct
#' @import stats
#' @export wilcox_test
#' @examples
#' M = wilcox_test()
#'
wilcox_test<-setClass(
    "wilcox_test",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_names='entity',
        params.paired='entity',
        params.paired_factor='character',
        # OUTPUTS
        outputs.statistic='entity.stato',
        outputs.p_value='entity',
        outputs.dof='entity.stato',
        outputs.significant='entity',
        outputs.conf_int='entity',
        outputs.estimates='data.frame'
    ),
    prototype = list(name='wilcoxon signed rank test',
        description='Applies the signed rank test to each feature to indicate significance, with (optional)
                                multiple-testing correction.',
        type="univariate",
        predicted='p_value',
        stato.id="STATO:0000304",

        params.factor_names=entity(name='Factor names',
            type='character',
            description='Names of sample_meta columns to use'
        ),

        params.alpha=entity.stato(name='Confidence level',
            stato.id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params.mtc=entity.stato(name='Multiple Test Correction method',
            stato.id='OBI:0200089',
            value='fdr',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        params.paired=entity(name='Apply paired test',
            value=FALSE,
            type='logical',
            description='TRUE/FALSE to apply paired test.'
        ),
        outputs.statistic=entity.stato(name='statistic',
            stato.id='STATO:0000176',
            type='numeric',
            description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.'
        ),
        outputs.p_value=entity.stato(name='p value',
            stato.id='STATO:0000175',
            type='numeric',
            description='the probability of observing the calculated t-statistic.'
        ),
        outputs.dof=entity.stato(name='degrees of freedom',
            stato.id='STATO:0000069',
            type='numeric',
            description='the number of degrees of freedom used to calculate the test statistic'
        ),
        outputs.significant=entity(name='Significant features',
            #stato.id='STATO:0000069',
            type='logical',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        ),
        outputs.conf_int=entity(name='Confidence interval',
            type='data.frame',
            description='confidence interval for t statistic'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model.apply",
    signature=c("wilcox_test",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        CN=colnames(X) # keep a copy of the original colnames
        y=dataset.sample_meta(D)[[M$factor_names]]
        L=levels(y)
        if (length(L)!=2) {
            stop('must have exactly two levels for this implmentation of t-statistic')
        }

        if (M$paired){
            # check that we have a pair for each sample,
            # if not then remove
            u=unique(D$sample_meta[[M$paired_factor]])
            out=character(0) # list of sample_id to remove
            for (k in u) {
                n=sum(D$sample_meta[[M$paired_factor]]==k) # number of samples (could be same class)
                if (n<2) {
                    out=c(out,k)
                }
                # if we have more than 2 then we need an even number.
                if (n%%2 != 0) {
                    out=c(out,k)
                }
                # check we have enough groups (must be two for ttest)
                ng=length(unique(D$sample_meta[[M$factor_names]][D$sample_meta[[M$paired_factor]]==k]))
                if (ng != 2) {
                    out=c(out,k)
                }

            }
            D$data=D$data[!(D$sample_meta[[M$paired_factor]] %in% out),]
            D$sample_meta=D$sample_meta[!(D$sample_meta[[M$paired_factor]] %in% out),]
            y=dataset.sample_meta(D)[[M$factor_names]]

            # sort the data by sample id so that theyre in the right order for paired ttest
            X=apply(D$data,2,function(x) {
                a=x[y==L[1]]
                b=x[y==L[2]]
                ay=y[y==L[1]]
                by=y[y==L[2]]
                a=a[order(ay)]
                b=b[order(by)]
                return(c(a,b))
            })

            # put back into dataset object
            D$data=as.data.frame(X)
            D$sample_meta[[M$factor_names]]=D$sample_meta[[M$factor_names]][order(D$sample_meta[[M$paired_factor]])]
            D$sample_meta[[M$paired_factor]]=D$sample_meta[[M$paired_factor]][order(D$sample_meta[[M$paired_factor]])]
            y=dataset.sample_meta(D)[[M$factor_names]]

            # check number per class
            # if less then 2 then remove
            FF=filter_na_count(threshold=2,factor_name=M$factor_names)
            FF=model.apply(FF,D)
            D=predicted(FF)

            # check equal numbers per class. if not equal then exclude.
            out=FF$count[,1]!=FF$count[,2]
            D$data=D$data[,!out]
        }



        X=D$data
        y=D$sample_meta[[M$factor_names]]

        output=apply(X,2,function(x) {
            a=unlist(wilcox.test(x[y==L[1]],x[y==L[2]],paired = M$paired,conf.int=TRUE)[c("statistic","p.value","parameter",'conf.int','estimate')])
        })

        temp=data.frame(row.names=CN) # make sure we get  result for all features, even if NA
        output=merge(temp,as.data.frame(t(output),stringsAsFactors = FALSE),by=0,all=TRUE,sort=FALSE)
        rownames(output)=output$Row.names
        output=output[,-1]
        output$p.value=p.adjust(output$p.value,method = param.value(M,'mtc'))
        output.value(M,'statistic')=output$statistic.W
        output.value(M,'p_value')=output$p.value
        output.value(M,'significant')=output$p.value<param.value(M,'alpha')
        M$conf_int=output[,3:4,drop=FALSE]
        colnames(M$conf_int)=c('lower','upper')
        if (M$paired) {
            M$estimates=output[,5,drop=FALSE]
            colnames(M$estimates)='estimated_mean_difference'
        } else {
            M$estimates=output[,5,drop=FALSE]
            colnames(M$estimates)=as.character('estimate.difference in location')
        }

        return(M)
    }
)


##### plots
#' plot histogram of p values
#'
#' plots a histogram of p values
#' @import struct
#' @export wilcox_p_hist
#' @examples
#' M = wilcox_p_hist()
#'
wilcox_p_hist<-setClass(
    "wilcox_p_hist",
    contains='chart',
    prototype = list(name='Histogram of p values',
        description='Histogram of p values',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("wilcox_p_hist",'wilcox_test'),
    definition=function(obj,dobj)
    {
        t=param.value(dobj,'alpha')
        A=log10(data.frame(p_value=dobj$'p_value'))
        A$sig=dobj$significant
        A$features=factor(A$sig,levels=c(FALSE,TRUE),labels=c('accepted','rejected'))

        out=ggplot(data=A, aes_(x=~p_value,fill=~features)) +
            geom_histogram(boundary=t,color='white') +
            xlab('log10(p-values)') +
            ylab('Count') +
            structToolbox:::scale_fill_Publication()+
            structToolbox:::theme_Publication(base_size = 12) +
            ggtitle('Wilcoxon signed rank test')+
            theme(panel.border = element_rect(linetype = "solid", fill = NA))

        po=ggplot_build(out)
        breaks=po$layout$panel_scales_x[[1]]$get_breaks()

        # add second axis with labels
        out=out+scale_x_continuous(breaks=breaks,sec.axis=dup_axis(labels=10^breaks,name='p-values'))

        return(out)
    }
)






