#' t-test model class
#'
#' t-test model class. Calculate t-test for all features in a DatasetExperiment
#'
#' @import struct
#' @import stats
#' @param ... slots and values for the new object
#' @return struct object
#' @export ttest
#' @examples
#' M = ttest()
#'
ttest = function(...) {
    out=.ttest()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.ttest<-setClass(
    "ttest",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        params_alpha='entity_stato',
        params_mtc='entity_stato',
        params_factor_names='entity',
        params_paired='entity',
        params_paired_factor='entity',
        # OUTPUTS
        outputs_t_statistic='entity_stato',
        outputs_p_value='entity',
        outputs_dof='entity_stato',
        outputs_significant='entity',
        outputs_conf_int='entity',
        outputs_estimates='data.frame'
    ),
    prototype = list(name='t-test',
        description='Applies the t-test to each feature to indicate significance, with (optional)
                                multiple-testing correction.',
        type="univariate",
        predicted='p_value',
        stato_id="STATO:0000304",

        params_factor_names=entity(name='Factor names',
            type='character',
            description='Names of sample_meta columns to use'
        ),

        params_alpha=entity_stato(name='Confidence level',
            stato_id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params_mtc=entity_stato(name='Multiple Test Correction method',
            stato_id='OBI:0200089',
            value='fdr',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        params_paired=entity(name='Apply paired t-test',
            value=FALSE,
            type='logical',
            description='TRUE/FALSE to apply paired t-test.'
        ),
        params_paired_factor=entity(name='Paired factor',
            value='NA',
            type='character',
            description='Factor name that encodes the sample id for pairing'
        ),
        outputs_t_statistic=entity_stato(name='t-statistic',
            stato_id='STATO:0000176',
            type='numeric',
            description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.'
        ),
        outputs_p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='numeric',
            description='the probability of observing the calculated t-statistic.'
        ),
        outputs_dof=entity_stato(name='degrees of freedom',
            stato_id='STATO:0000069',
            type='numeric',
            description='the number of degrees of freedom used to calculate the test statistic'
        ),
        outputs_significant=entity(name='Significant features',
            #stato_id='STATO:0000069',
            type='logical',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        ),
        outputs_conf_int=entity(name='Confidence interval',
            type='data.frame',
            description='confidence interval for t statistic'
        )
    )
)

#' @param ... slots and values for the new object
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("ttest",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        CN=colnames(X) # keep a copy of the original colnames
        y=D$sample_meta[[M$factor_names]]
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
            D$data=D$data[!(D$sample_meta[[M$paired_factor]] %in% out),]
            D$sample_meta=D$sample_meta[!(D$sample_meta[[M$paired_factor]] %in% out),]
            y=D$sample_meta[[M$factor_names]]

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

            # put back into DatasetExperiment object
            D$data=as.data.frame(X)
            D$sample_meta[[M$factor_names]]=D$sample_meta[[M$factor_names]][order(D$sample_meta[[M$paired_factor]])]
            D$sample_meta[[M$paired_factor]]=D$sample_meta[[M$paired_factor]][order(D$sample_meta[[M$paired_factor]])]
            y=D$sample_meta[[M$factor_names]]

            # check number per class
            # if less then 2 then remove
            FF=filter_na_count(threshold=2,factor_name=M$factor_names)
            FF=model_apply(FF,D)
            D=predicted(FF)

            # check equal numbers per class. if not equal then exclude.
            IN=rownames(FF$count)[(FF$count[,1]==FF$count[,2]) & (FF$count[,1]>2) & (FF$count[,2]>2)]
            D$data=D$data[,IN]
            D$variable_meta=D$variable_meta[IN,]

            estimate_name='estimate.mean of the differences'
        }



        X=D$data
        y=D$sample_meta[[M$factor_names]]

        output=lapply(X,function(x) {
            a=tryCatch({
                g=unlist(t.test(x[y==L[1]],x[y==L[2]],paired = M$paired)[c("statistic","p.value","parameter",'conf.int','estimate')])
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
setMethod(f="as_data_frame",
    signature=c("ttest"),
    definition=function(M) {
        out=data.frame('t_statistic'=M$t_statistic,
            't_p_value'=M$p_value,
            't_significant'=M$significant)
        out=cbind(out,M$estimates,M$conf_int)
    }
)




