#' @eval get_description('kw_rank_sum')
#' @examples
#' D = iris_DatasetExperiment()
#' M = kw_rank_sum(factor_names='Species')
#' M = model_apply(M,D)
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
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='enum_stato',
        factor_names='entity',
        # OUTPUTS
        test_statistic='entity',
        p_value='entity',
        dof='entity_stato',
        significant='entity',
        estimates='data.frame'
    ),
    prototype = list(name='Kruskal-Wallis rank sum test',
        description=paste0('The Kruskal-Wallis test is a univariate ',
        'hypothesis testing method that allows multiple (n>=2) groups to be ',
        'compared without making the assumption that values are normally ',
        'distributed. It is the non-parametric equivalent of a 1-way ANOVA. ',
        'The test is applied to all variables/features individually, and ',
        'multiple test corrected p-values are computed to indicate the ',
        'significance of variables/features.'),
        type="univariate",
        predicted='p_value',
        .params=c('alpha','mtc','factor_names'),
        .outputs=c('test_statistic','p_value','dof','significant','estimates'),
        stato_id='STATO:0000094',
        factor_names=ents$factor_names,

        alpha=ents$alpha,
        mtc=ents$mtc,
        test_statistic=entity(name='test statistic',
            type='data.frame',
            description='the value of the calculated statistic which is converted to a p-value when compared to a chi2-distribution.'
        ),
        p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated statistic.'
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
#' @eval get_description('kw_p_hist')
#' @import struct
#' @examples
#' C = kw_p_hist()
#' @export kw_p_hist
kw_p_hist = function(...) {
    out=struct::new_struct('kw_p_hist',...)
    return(out)
}


.kw_p_hist<-setClass(
    "kw_p_hist",
    contains='chart',
    prototype = list(name='Histogram of p values',
        description='A histogram of the p-values computed by the kruskal-wallis method',
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
            scale_fill_Publication()+
            theme_Publication(base_size = 12) +
            ggtitle('Kruskal-Wallis rank sum')+
            theme(panel.border = element_rect(linetype = "solid", fill = NA))

        po=ggplot_build(out)
        breaks=po$layout$panel_scales_x[[1]]$get_breaks()

        # add second axis with labels
        out=out+scale_x_continuous(breaks=breaks,sec.axis=dup_axis(labels=10^breaks,name='p-values'))

        return(out)
    }
)





