#' @eval get_description('wilcox_test')
#' @return struct object
#' @export wilcox_test
#' @examples
#' M = wilcox_test(factor_name='Class')
#'
wilcox_test = function(alpha=0.05,mtc='fdr',factor_names,paired=FALSE,paired_factor=character(0),...) {
    out=struct::new_struct('wilcox_test',
        alpha=alpha,
        mtc=mtc,
        factor_names=factor_names,
        paired=paired,
        paired_factor=paired_factor,
        ...)
    return(out)
}


.wilcox_test<-setClass(
    "wilcox_test",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity',
        mtc='enum',
        factor_names='entity',
        paired='entity',
        paired_factor='entity',
        # OUTPUTS
        statistic='entity',
        p_value='entity',
        dof='entity',
        significant='entity',
        conf_int='entity',
        estimates='data.frame'
    ),
    prototype = list(name='wilcoxon signed rank test',
        description=paste0('A Mann-Whitney-Wilcoxon signed rank test compares ,',
        'the ranks of values in two groups. It is the non-parametric equivalent ',
        'of a t-test. Multiple test corrected p-values are computed as ',
        'indicators of significance for each variable/feature.'),
        type="univariate",
        predicted='p_value',
        ontology="STATO:0000092",
        .params=c('alpha','mtc','factor_names','paired','paired_factor'),
        .outputs=c('statistic','p_value','dof','significant','conf_int','estimates'),

        factor_names=ents$factor_name,

        alpha=ents$alpha,
        mtc=ents$mtc,
        paired=entity(name='Apply paired test',
            value=FALSE,
            type='logical',
            description='Apply a paired test.'
        ),
        paired_factor=entity(
            name='Paired factor',
            description='The factor name containing sample ids for paired data.',
            type='character',
            value=character(0),
            max_length=1
        ),
        statistic=entity(name='statistic',
            ontology='STATO:0000176',
            type='numeric',
            description='the value of the calculated statistic which is converted to a p-value.'
        ),
        p_value=entity(name='p value',
            ontology='STATO:0000175',
            type='numeric',
            description='the probability of observing the calculated t-statistic.'
        ),
        dof=entity(name='degrees of freedom',
            ontology='STATO:0000069',
            type='numeric',
            description='the number of degrees of freedom used to calculate the test statistic'
        ),
        significant=entity(name='Significant features',
            #ontology='STATO:0000069',
            type='logical',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        ),
        conf_int=entity(name='Confidence interval',
            type='data.frame',
            description='confidence interval for t statistic'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("wilcox_test",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        CN=colnames(X) # keep a copy of the original colnames
        y=D$sample_meta[[M$factor_names]]
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
            #D$data=D$data[!(D$sample_meta[[M$paired_factor]] %in% out),]
            #D$sample_meta=D$sample_meta[!(D$sample_meta[[M$paired_factor]] %in% out),]
            D=D[!(D$sample_meta[[M$paired_factor]] %in% out),]
            y=D$sample_meta[[M$factor_names]]
            
            # sort the data by sample id so that theyre in the right order for paired ttest
            temp=D$sample_meta[order(D$sample_meta[[M$factor_names]],D$sample_meta[[M$paired_factor]]),]
            D=D[rownames(temp),]
            
            # check number per class
            # if less then 2 then remove
            FF=filter_na_count(threshold=2,factor_name=M$factor_names)
            FF=model_apply(FF,D)
            D=predicted(FF)
            
            # check equal numbers per class. if not equal then exclude.
            IN=rownames(FF$count)[(FF$count[,1]==FF$count[,2]) & (FF$count[,1]>2) & (FF$count[,2]>2)]
            D=D[,IN]
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
        output$p.value=p.adjust(output$p.value,method = param_value(M,'mtc'))
        if (M$paired) {
            output_value(M,'statistic')=output$statistic.V
        } else {
            output_value(M,'statistic')=output$statistic.W
        }
        output_value(M,'p_value')=output$p.value
        output_value(M,'significant')=output$p.value<param_value(M,'alpha')
        M$conf_int=output[,3:4,drop=FALSE]
        colnames(M$conf_int)=c('lower','upper')
        if (M$paired) {
            M$estimates=output[,5,drop=FALSE]
            colnames(M$estimates)='estimate.(pseudo)median'
        } else {
            M$estimates=output[,5,drop=FALSE]
            colnames(M$estimates)=as.character('estimate.difference in location')
        }

        return(M)
    }
)


##### plots
#' @eval get_description('wilcox_p_hist')
#' @import struct
#' @export wilcox_p_hist
#' @examples
#' M = wilcox_p_hist()
#'
wilcox_p_hist = function(...) {
    out=struct::new_struct('wilcox_p_hist',...)
    return(out)
}


.wilcox_p_hist<-setClass(
    "wilcox_p_hist",
    contains='chart',
    prototype = list(name='Histogram of p values',
        description='A histogram of p values for the wilcoxon signed rank test',
        type="histogram"
    )
)


#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("wilcox_p_hist",'wilcox_test'),
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
            ggtitle('Wilcoxon signed rank test')+
            theme(panel.border = element_rect(linetype = "solid", fill = NA))

        po=ggplot_build(out)
        breaks=po$layout$panel_scales_x[[1]]$get_breaks()

        # add second axis with labels
        out=out+scale_x_continuous(breaks=breaks,sec.axis=dup_axis(labels=10^breaks,name='p-values'))

        return(out)
    }
)



#' @export
#' @template as_data_frame
setMethod(f="as_data_frame",
    signature=c("wilcox_test"),
    definition=function(M) {
        out=data.frame('w_statistic'=M$statistic,
            'w_p_value'=M$p_value,
            'w_significant'=M$significant)
        out=cbind(out,M$estimates,M$conf_int)
    }
)



