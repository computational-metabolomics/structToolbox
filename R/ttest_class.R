#' t-test model class
#'
#' t-test model class. Calculate t-test for all features in a dataset
#'
#' @import struct
#' @import stats
#' @export ttest
#' @examples
#' M = ttest()
#'
ttest<-setClass(
    "ttest",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_names='entity',
        params.paired='entity',
        params.paired_factor='entity',
        # OUTPUTS
        outputs.t_statistic='entity.stato',
        outputs.p_value='entity',
        outputs.dof='entity.stato',
        outputs.significant='entity',
        outputs.conf_int='entity',
        outputs.estimates='data.frame'
    ),
    prototype = list(name='t-test',
        description='Applies the t-test to each feature to indicate significance, with (optional)
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
        params.paired=entity(name='Apply paired t-test',
            value=FALSE,
            type='logical',
            description='TRUE/FALSE to apply paired t-test.'
        ),
        params.paired_factor=entity(name='Paired factor',
            value='NA',
            type='character',
            description='Factor name that encodes the sample id for pairing'
        ),
        outputs.t_statistic=entity.stato(name='t-statistic',
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
    signature=c("ttest",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        CN=colnames(X) # keep a copy of the original colnames
        y=dataset.sample_meta(D)[[M$factor_names]]
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

        output=as.data.frame(output)
        temp=data.frame(row.names=CN) # make sure we get  result for all features, even if NA
        output=merge(temp,as.data.frame(t(output),stringsAsFactors = FALSE),by=0,all=TRUE,sort=FALSE)
        rownames(output)=output$Row.names
        output=output[,-1]
        output$p.value=p.adjust(output$p.value,method = param.value(M,'mtc'))
        output.value(M,'t_statistic')=output$statistic.t
        output.value(M,'p_value')=output$p.value
        output.value(M,'dof')=output$parameter.df
        output.value(M,'significant')=output$p.value<param.value(M,'alpha')
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









