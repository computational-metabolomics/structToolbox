#' @eval get_description('HSDEM')
#' @include mixed_effect_class.R HSD_class.R
#' @export HSDEM
#' @examples
#' D = iris_DatasetExperiment()
#' D$sample_meta$id=rownames(D) # dummy id column
#' M = HSDEM(formula = y~Species+ Error(id/Species))
#' M = model_apply(M,D)
#'
HSDEM = function(alpha=0.05,mtc='fdr',formula,...) {
    out=struct::new_struct('HSDEM',
        alpha=alpha,
        mtc=mtc,
        formula=formula,
        ...)
    return(out)
}


.HSDEM<-setClass(
    "HSDEM",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity',
        mtc='enum',
        formula='entity',

        # OUTPUTS
        p_value='entity',
        significant='entity'
    ),
    prototype = list(name="Tukey's Honest Significant Difference using estimated marginal means",
        description=paste0("Tukey's HSD post hoc test is a modified t-test ",
            'applied for all features to all pairs of levels in a factor. ',
            'It is used to determine which groups are different (if any). A multiple ',
            'test corrected p-value is computed to indicate which groups are ',
            'significantly different to the others for each feature. ',
            'For mixed effects models estimated marginal means are used.'),
        type="univariate",
        predicted='p_value',
        ontology="STATO:0000187",
        libraries=c('emmeans','nlme'),
        .params=c('alpha','mtc','formula'),
        .outputs=c('p_value','significant'),

        alpha=ents$alpha,
        mtc=ents$mtc,
        formula=ents$formula,
        p_value=ents$p_value,
        significant=ents$significant
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

        # lapply (not apply) so the per-feature results are not simplified
        output=lapply(X,function(x) {
            temp=y
            temp[[var_names_1]]=scale(x,center = TRUE,scale=TRUE)

            testlm=tryCatch({ # if any warnings/messages set p-values to NA as unreliable
                nlme::lme(lmer_formula$f,random=lmer_formula$random,method='ML',data=temp,na.action=na.omit)
            }, warning = function(w) {
                NA
            }, message = function(m) {
                NA
            }, error   = function(e) {
                NA
            })

            # named p-values for the contrasts of every term; failed terms are left out
            pv=numeric(0)
            if (is(testlm,'lme')) {
                for (k in seq_along(FF)) {
                    A=tryCatch({
                        # emmeans notes about interactions are not failures
                        suppressMessages(as.data.frame(
                            pairs(emmeans::emmeans(testlm,FF[[k]],data=temp))))
                    }, warning  = function(w) {
                        NULL
                    }, error    = function(e) {
                        NULL
                    })
                    if (is.data.frame(A)) {
                        cn=as.character(A$contrast)
                        if (length(FF)>1) {
                            # prefix the term so contrast names are unique
                            cn=paste0(paste(FF[[k]],collapse=':'),': ',cn)
                        }
                        pv=c(pv,setNames(A$p.value,cn))
                    }
                }
            }
            return(pv)
        })

        # line up contrasts by name; missing terms become NA
        # (most complete feature first, so the column order is by term)
        cn=unique(unlist(lapply(output[order(-lengths(output))],names)))
        p_value=do.call("rbind",lapply(output,function(x) unname(x[cn])))
        p_value=data.frame(p_value,check.names=FALSE)
        colnames(p_value)=cn
        rownames(p_value)=colnames(X)

        # fdr correct
        M$p_value=as.data.frame(apply(p_value,2,p.adjust,method=M$mtc))

        M$significant=as.data.frame(M$p_value<M$alpha)

        # reset contrasts
        options(O)

        return(M)
    }
)






