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
    ...) {
    out=struct::new_struct('ttest',
        alpha=alpha,
        mtc=mtc,
        factor_names=factor_names,
        paired=paired,
        paired_factor=paired_factor,
        equal_variance=equal_variance,
        ...)
    return(out)
}


.ttest<-setClass(
    "ttest",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='enum_stato',
        factor_names='entity',
        paired='entity',
        paired_factor='entity',
        equal_variance='entity',
        # OUTPUTS
        t_statistic='entity_stato',
        p_value='entity',
        dof='entity_stato',
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
        stato_id="STATO:0000304",
        .params=c('alpha','mtc','factor_names','paired','paired_factor','equal_variance'),
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
        
        t_statistic=entity_stato(name='t-statistic',
            stato_id='STATO:0000176',
            type='data.frame',
            description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.'
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
            stop('must have exactly two levels for this implementation of t-statistic')
        }
        
        
        if (M$paired){
            estimate_name='estimate.mean of the differences'
        } else {
            estimate_name='estimate'
        }
        
        X=D$data
        y=D$sample_meta[[M$factor_names]]
        
        output=lapply(X,function(x) {
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
                    t.test(
                        A,
                        B,
                        paired = M$paired,
                        var.equal=M$equal_variance
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
        # ensure outputs are in the correct order (TODO: update to data.frame with rownames)
        output=output[CN,]
        output$p.value='p_value'=p.adjust(output$p.value,method = param_value(M,'mtc'))
        output_value(M,'t_statistic')=data.frame('t_statistic'=output$statistic.t,row.names = CN)
        output_value(M,'p_value')=data.frame('p_value'=output$p.value,row.names = CN)
        output_value(M,'dof')=output$parameter.df
        output_value(M,'significant')=data.frame('significant'=output$p.value<param_value(M,'alpha'),row.names=CN)
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
        df=cbind(M$t_statistic,M$p_value,M$significant,M$estimates,M$conf_int)
        return(df)
    }
)




