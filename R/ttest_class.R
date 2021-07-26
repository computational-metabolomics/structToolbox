#' @eval get_description('ttest')
#' @export ttest
#' @examples
#' M = ttest(factor_name='Class')
#'
ttest = function(
    alpha=0.05,
    mtc='fdr',
    factor_names,
    paired=FALSE,
    paired_factor=character(0),
    equal_variance=FALSE,
    conf_level=0.95,
    ...) {
    out=struct::new_struct('ttest',
        alpha=alpha,
        mtc=mtc,
        factor_names=factor_names,
        paired=paired,
        paired_factor=paired_factor,
        equal_variance=equal_variance,
        conf_level=conf_level,
        ...)
    return(out)
}


.ttest<-setClass(
    "ttest",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity',
        mtc='enum',
        factor_names='entity',
        paired='entity',
        paired_factor='entity',
        equal_variance='entity',
        conf_level='entity',
        # OUTPUTS
        t_statistic='entity',
        p_value='entity',
        dof='entity',
        significant='entity',
        conf_int='entity',
        estimates='data.frame'
    ),
    prototype = list(name='t-test',
        description=paste0('A t-test compares the means of two factor levels. ',
        'Multiple-test corrected p-values are used to indicate the significance ',
        'of the computed difference for all features.'),
        type="univariate",
        predicted='p_value',
        ontology="STATO:0000304",
        .params=c('alpha','mtc','factor_names','paired','paired_factor','equal_variance','conf_level'),
        .outputs=c('t_statistic','p_value','dof','significant','conf_int','estimates'),

        factor_names=ents$factor_names,

        alpha=ents$alpha,
        mtc=ents$mtc,
        paired=entity(name='Apply paired t-test',
            value=FALSE,
            type='logical',
            description='Apply a paired t-test.'
        ),
        paired_factor=entity(name='Paired factor',
            value='NA',
            type='character',
            description='The factor name that encodes the sample id for pairing'
        ),
        equal_variance=entity(name = 'Equal variance',
            description = c(
                "TRUE" = paste0('The variance of each group is ',
                'treated as being equal using the pooled variance to estimate ',
                    'the variance.'),
                "FALSE" = paste0('The variance of each group is not assumed to ',
                'be equal and the Welch (or Satterthwaite) approximation is ',
                'used.')
            ),
            value = FALSE,
            type = 'logical',
            max_length = 1
        ),
        
        t_statistic=entity(name='t-statistic',
            ontology='STATO:0000176',
            type='numeric',
            description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.'
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
        ),
        conf_level = entity(
            name = 'Confidence level',
            description = 'The confidence level of the interval.',
            type='numeric',
            value=0.95,
            max_length = 1
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("ttest",'DatasetExperiment'),
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
            stop('must have exactly two levels for this implmentation of t-statistic')
        }

        estimate_name='estimate'
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

            estimate_name='estimate.mean of the differences'
        }



        X=D$data
        y=D$sample_meta[[M$factor_names]]

        output=lapply(X,function(x) {
            a=tryCatch({
                g=unlist(
                    t.test(
                        x[y==L[1]],
                        x[y==L[2]],
                        paired = M$paired,
                        var.equal=M$equal_variance,
                        conf.level=M$conf_level
                   )[c("statistic","p.value","parameter",'conf.int','estimate')]
                )
                return(g)
            },warning=function(w) {
                g = NA
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
        output$p.value=p.adjust(output$p.value,method = param_value(M,'mtc'))
        output_value(M,'t_statistic')=output$statistic.t
        output_value(M,'p_value')=output$p.value
        output_value(M,'dof')=output$parameter.df
        output_value(M,'significant')=output$p.value<param_value(M,'alpha')
        M$conf_int=output[,4:5,drop=FALSE]
        colnames(M$conf_int)=c('lower','upper')
        if (M$paired) {
            M$estimates=output[,6,drop=FALSE]
            colnames(M$estimates)='estimated_mean_difference'
        } else {
            M$estimates=output[,6:7,drop=FALSE]
            colnames(M$estimates)=as.character(interaction('estimate.mean',L))
        }

        return(M)
    }
)




#' @export
#' @template as_data_frame
setMethod(f="as_data_frame",
    signature=c("ttest"),
    definition=function(M) {
        out=data.frame('t_statistic'=M$t_statistic,
            't_p_value'=M$p_value,
            't_significant'=M$significant)
        out=cbind(out,M$estimates,M$conf_int)
    }
)




