#' kruskal-wallis model class
#'
#'  Calculate kw-test for all features in a DatasetExperiment.
#'  A non-parametric 1-way ANOVA.
#'
#' @examples
#' M = kw_rank_sum()
#' @param alpha The p-value threshold. Default alpha = 0.05.
#' @param mtc Multiple test correction method passed to \code{p.adjust}. Default mtc = 'fdr'.
#' @param formula The formula to use. See \code{lm} for details.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export kw_rank_sum
kw_rank_sum = function(alpha=0.05,mtc='fdr',factor_names,...) {
    out=struct::new_struct('kw_rank_sum',
        alpha=alpha,
        mtc=mtc,
        factor_names=factor_names,
        ...)
    return(out)
}


.kw_rank_sum<-setClass(
    "kw_rank_sum",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='entity_stato',
        factor_names='entity',
        # OUTPUTS
        test_statistic='entity',
        p_value='entity',
        dof='entity_stato',
        significant='entity',
        estimates='data.frame'
    ),
    prototype = list(name='kruskal-wallis rank sum test',
        description='Applies the kw rank sum test to each feature to indicate significance, with (optional)
                                multiple-testing correction.',
        type="univariate",
        predicted='p_value',
        .params=c('alpha','mtc','factor-names'),
        .outputs=c('test_statistic','p_value','dof','significant','estimates'),

        factor_names=entity(name='Factor names',
            type='character',
            description='Names of sample_meta columns to use'
        ),

        alpha=entity_stato(name='Confidence level',
            stato_id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        mtc=entity_stato(name='Multiple Test Correction method',
            stato_id='OBI:0200089',
            value='fdr',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        test_statistic=entity(name='test statistic',
            type='data.frame',
            description='the value of the calculated statistic which is converted to a p-value when compared to a chi2-distribution.'
        ),
        p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated t-statistic.'
        ),
        dof=entity_stato(name='degrees of freedom',
            stato_id='STATO:0000069',
            type='numeric',
            description='the number of degrees of freedom used to calculate the test statistic'
        ),
        significant=entity(name='Significant features',
            #stato_id='STATO:0000069',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("kw_rank_sum",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        CN=colnames(X) # keep a copy of the original colnames
        y=D$sample_meta[[M$factor_names]]
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

        output$p.value=p.adjust(output$p.value,method = param_value(M,'mtc'))

        output_value(M,'test_statistic')=data.frame(output$statistic)
        colnames(M$test_statistic)=M$factor_names

        output_value(M,'p_value')=data.frame(output$p.value)
        colnames(M$p_value)=M$factor_names

        output_value(M,'dof')=output$parameter

        output_value(M,'significant')=data.frame(output$p.value<param_value(M,'alpha'))
        colnames(M$significant)=M$factor_names

        return(M)
    }
)


##### plots
#' plot histogram of p values
#'
#' plots a histogram of p values
#' @import struct
#' @examples
#' C = kw_p_hist()
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export kw_p_hist
kw_p_hist = function(...) {
    out=struct::new_struct('kw_p_hist',...)
    return(out)
}


.kw_p_hist<-setClass(
    "kw_p_hist",
    contains='chart',
    prototype = list(name='Histogram of p values',
        description='Histogram of p values',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("kw_p_hist",'kw_rank_sum'),
    definition=function(obj,dobj)
    {
        t=param_value(dobj,'alpha')
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





