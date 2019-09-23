#' kruskal-wallis model class
#'
#' kruskal-wallis model class. Calculate kw-test for all features in a dataset.
#' A non-parametric 1-way ANOVA.
#'
#' @import struct
#' @import stats
#' @export kw_rank_sum
kw_rank_sum<-setClass(
    "kw_rank_sum",
    contains=c('method'),
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_names='entity',
        # OUTPUTS
        outputs.test_statistic='entity',
        outputs.p_value='entity',
        outputs.dof='entity.stato',
        outputs.significant='entity',
        outputs.estimates='data.frame'
    ),
    prototype = list(name='kruskal-wallis rank sum test',
        description='Applies the kw rank sum test to each feature to indicate significance, with (optional)
                                multiple-testing correction.',
        type="univariate",
        predicted='p_value',

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
        outputs.test_statistic=entity(name='test statistic',
            type='numeric',
            description='the value of the calculated statistic which is converted to a p-value when compared to a chi2-distribution.'
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
        )
    )
)

#' @export
#' @template method_apply
setMethod(f="method.apply",
    signature=c("kw_rank_sum",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        CN=colnames(X) # keep a copy of the original colnames
        y=dataset.sample_meta(D)[[M$factor_names]]
        L=levels(y)

        X=D$data
        y=D$sample_meta[[M$factor_names]]

        output=apply(X,2,function(x) {
            a=unlist(kruskal.test(x,y)[c("statistic","p.value","parameter")])
        })

        temp=data.frame(row.names=CN) # make sure we get  result for all features, even if NA
        output=merge(temp,as.data.frame(t(output),stringsAsFactors = FALSE),by=0,all=TRUE,sort=FALSE)
        rownames(output)=output$Row.names
        output=output[,-1]
        output$p.value=p.adjust(output$p.value,method = param.value(M,'mtc'))
        output.value(M,'test_statistic')=output$statistic
        output.value(M,'p_value')=output$p.value
        output.value(M,'dof')=output$parameter
        output.value(M,'significant')=output$p.value<param.value(M,'alpha')

        return(M)
    }
)


##### plots
#' plot histogram of p values
#'
#' plots a histogram of p values
#' @import struct
#' @export kw_p_hist
kw_p_hist<-setClass(
    "kw_p_hist",
    contains='chart',
    prototype = list(name='Histogram of p values',
        description='Histogram of p values',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("kw_p_hist",'kw_rank_sum'),
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
            ggtitle('Kruskal-Wallis rank sum')+
            theme(panel.border = element_rect(linetype = "solid", fill = NA))

        po=ggplot_build(out)
        breaks=po$layout$panel_scales_x[[1]]$get_breaks()

        # add second axis with labels
        out=out+scale_x_continuous(breaks=breaks,sec.axis=dup_axis(labels=10^breaks,name='p-values'))

        return(out)
    }
)





