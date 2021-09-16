#' @eval get_description('wilcox_test')
#' @return struct object
#' @export wilcox_test
#' @examples
#' M = wilcox_test(factor_name='Class')
#'
wilcox_test = function(
    alpha=0.05,
    mtc='fdr',
    factor_names,
    paired=FALSE,
    paired_factor=character(0),
    conf_level=0.95,
    ...) {
    out=struct::new_struct('wilcox_test',
        alpha=alpha,
        mtc=mtc,
        factor_names=factor_names,
        paired=paired,
        paired_factor=paired_factor,
        conf_level=conf_level,
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
        conf_level='entity',
        # OUTPUTS
        statistic='entity',
        p_value='entity',
        dof='entity',
        significant='entity',
        conf_int='entity',
        estimates='entity'
    ),
    prototype = list(name='wilcoxon signed rank test',
        description=paste0('A Mann-Whitney-Wilcoxon signed rank test compares ,',
        'the ranks of values in two groups. It is the non-parametric equivalent ',
        'of a t-test. Multiple test corrected p-values are computed as ',
        'indicators of significance for each variable/feature.'),
        type="univariate",
        predicted='p_value',
        ontology="STATO:0000092",
        .params=c('alpha','mtc','factor_names','paired','paired_factor','conf_level'),
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
            type='data.frame',
            description='the value of the calculated statistic which is converted to a p-value.'
        ),
        p_value=entity(name='p value',
            ontology='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated t-statistic.'
        ),
        dof=entity(name='degrees of freedom',
            ontology='STATO:0000069',
            type='numeric',
            description='the number of degrees of freedom used to calculate the test statistic'
        ),
        significant=entity(name='Significant features',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        ),
        conf_int=entity(name='Confidence interval',
            type='data.frame',
            description='confidence interval for t statistic'
        ),
        conf_level = entity(
            name = 'Confidence level',
            description = 'The confidence level of the interval.',
            type='numeric',
            value=0.95,
            max_length = 1
        ),
        estimates=entity(
            name = 'Estimates',
            description = 'The group estimates used when computing the statistic.',
            type='data.frame'
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
        
        # convert to factor if it isn't one already
        if (!is(y,'factor')) {
            y=factor(y)
        }
        
        L=levels(y)
        if (length(L)!=2) {
            stop('must have exactly two levels for this implementation of wilcox_test')
        }
        
        
        if (M$paired){
            estimate_name='estimate.mean of the differences'
        } else {
            estimate_name='estimate'
        }
        
        X=D$data
        y=D$sample_meta[[M$factor_names]]

        output=apply(X,2,function(x) {
            a=tryCatch({
                
                # check for pairs if required
                if (M$paired) {
                    # get group A
                    dfA=data.frame(val=x[y==L[1]],id=D$sample_meta[y==L[1],M$paired_factor])
                    # get group B
                    dfB=data.frame(val=x[y==L[2]],id=D$sample_meta[y==L[2],M$paired_factor])
                    # merge
                    Z = merge(dfA,dfB,by='id') # will exclude any sample without a matching pair in sample list
                    # omit pairs with an NA
                    Z = na.omit(Z) # excludes any pair with at least one NA
                    
                    # check for at least 3 pairs
                    if (nrow(Z)<3) {
                        stop('not enough pairs')
                    }
                    
                    #extract for t-stat
                    A = Z$val.x
                    B = Z$val.y
                } else {
                    A = x[y==L[1]]
                    B = x[y==L[2]]
                }
                
                g=unlist(
                    wilcox.test(
                        A,
                        B,
                        paired = M$paired,
                        exact=FALSE,
                        conf.level=M$conf_level,
                        conf.int=TRUE
                    )[c("statistic","p.value","parameter",'conf.int','estimate')]
                )
                return(g)
            }, error=function(e) {
                g = NA
                return(g)
            }
            )
        })

        # replace na with vector of na the correct length/names
        na=which(is.na(output))
        
        if (length(na)>0) {
            notna=which(!is.na(output))
            torep=output[[notna[1]]] # the first output that worked as expected
            torep[1:length(torep)]=NA # populate with NA
            output[na]=rep(list(torep),length(na))
            
        }
        
        output=as.data.frame(output,check.names=FALSE)
        temp=data.frame(row.names=CN) # make sure we get  result for all features, even if NA
        output=merge(temp,as.data.frame(t(output),stringsAsFactors = FALSE),by=0,all=TRUE,sort=FALSE)
        rownames(output)=output$Row.names
        output=output[,-1]
        # ensure outputs are in the correct order (TODO: update to data.frame with rownames)
        output=output[CN,]
        output$p.value=p.adjust(output$p.value,method = param_value(M,'mtc'))
        if (M$paired) {
            output_value(M,'statistic')=data.frame('v_statistic'=output$statistic.V,row.names=CN)
        } else {
            output_value(M,'statistic')=data.frame('w_statistic'=output$statistic.W,row.names=CN)
        }
        output_value(M,'p_value')=data.frame('p_value'=output$p.value,row.names=CN)
        output_value(M,'significant')=data.frame('significant'=output$p.value<param_value(M,'alpha'),row.names=CN)
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



