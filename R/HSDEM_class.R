#' HSD model class using estimated marginal means
#'
#' HSD model class using estimate marginal means
#'
#' @import struct
#' @import stats
#' @import emmeans
#' @import nlme
#' @include mixed_effect_class.R HSD_class.R
#' @export HSDEM
#' @examples
#' M = HSDEM()
HSDEM = function(...) {
    out=.HSDEM()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.HSDEM<-setClass(
    "HSDEM",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        params_alpha='entity_stato',
        params_mtc='entity_stato',
        params_formula='entity',

        # OUTPUTS
        outputs_p_value='entity_stato',
        outputs_significant='entity'
    ),
    prototype = list(name='Tukey Honest Significant Difference using estimated marginal means',
        description='Tukey HSD post hoc tests for mixed effects models using estimated marginal means',
        type="univariate",
        predicted='p_value',
        stato_id="STATO:0000187",

        params_alpha=entity_stato(name='Confidence level',
            stato_id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params_mtc=entity_stato(name='Multiple Test Correction method',
            stato_id='OBI:0200089',
            value='none',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        params_formula=entity(name='Formula',
            value=y~x,
            type='formula',
            description='The formula to use'
        ),
        outputs_p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated t-statistic.'
        ),
        outputs_significant=entity(name='Significant features',
            #stato_id='STATO:0000069',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("HSDEM",'DatasetExperiment'),
    definition=function(M,D) {
        X=D$data
        lmer_formula=aov2lme(M$formula)
        var_names=all.vars(M$formula)
        var_names_1=var_names[1]
        var_names=var_names[-1]
        y=D$sample_meta[var_names]

        # set the contrasts
        O=options('contrasts') # keep the old ones
        options(contrasts = c("contr.sum","contr.poly"))

        # attempt to detect within factors
        within=which(var_names %in% all.names(M$formula)[which('Error'== all.names(M$formula))+2])
        if (length(within)>0) {
            var_names_ex=var_names[-within]
        } else {
            var_names_ex=var_names
        }

        FF=full_fact(var_names_ex)
        FF=apply(FF,1,function(x) var_names_ex[x==1])
        FF=FF[-1]

        output=apply(X,2,function(x) {
            temp=y
            temp[[var_names_1]]=scale(x,center = TRUE,scale=TRUE)

            dona=FALSE

            testlm=tryCatch({ # if any warnings/messages set p-values to NA as unreliable
                LM=lme(lmer_formula$f,random=lmer_formula$random,method='ML',data=temp,na.action=na.omit)
            }, warning = function(w) {
                NA
            }, message = function(m) {
                NA
            }, error   = function(e) {
                NA
            })

            output2=list()
            for (k in 1:length(FF)) {
                if (!is.na(testlm[[1]])) {
                    testhsd=tryCatch({
                        output2[[k]]=as.data.frame(
                                pairs(
                                emmeans::emmeans(LM,FF[[k]],
                                data=temp)))
                    }, warning  = function(w) {
                        NA
                    } , message = function(m) {
                        NA
                    }, error    = function(e) {
                        NA
                    })

                    if (!is.data.frame(testhsd[1])) {
                        output2[[k]]=NA
                    }
                } else {
                    output2[[k]]=NA
                }
            }

            return(output2)
        })

        p_value=lapply(output,function(x) {
            x=as.data.frame(x)
            return(x$p.value)
        })
        ln=length(p_value[[1]])
        p_value=lapply(p_value,function(x){
            if (length(x)!=ln) {
                return(p_value[[1]]*NA)
            } else {
                return(x)
            }
        })

        p_value=do.call("rbind",p_value)
        p_value=data.frame(p_value)
        colnames(p_value)=as.data.frame(output[[1]])$contrast

        # fdr correct
        M$p_value=as.data.frame(apply(p_value,2,p.adjust,method=M$mtc))

        M$significant=as.data.frame(M$p_value<M$alpha)

        # reset contrasts
        options(O)

        return(M)
    }
)






