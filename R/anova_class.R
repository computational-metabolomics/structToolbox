#' ANOVA
#'
#' Applies ANOVA to each feature in a DatasetExperiment object.
#'
#' @import struct
#' @import stats
#'
#' @templateVar paramNames c('alpha','mtc','formula')
#' @template common_params
#'
#' @examples
#' D = iris_DatasetExperiment()
#' M = ANOVA(formula=y~Species)
#' M = model_apply(M,D)
#'
#' @include entity_objects.R
#' @param alpha The p-value threshold. Default alpha = 0.05.
#' @param mtc Multiple test correction method passed to \code{p.adjust}. Default mtc = 'fdr'.
#' @param formula The formula to use for ANOVA. See \code{lm} for details.
#' @param ss_type The type of sum of squares to use. Can be I, II or III. Default is ss_type = 'III'.
#' @param ... additional slots and values passed to struct_class
#' @return ANOVA object
#' @export ANOVA
ANOVA = function(alpha=0.05,mtc='fdr',formula,ss_type='III',...) {
    out=struct::new_struct('ANOVA',
        alpha=alpha,
        mtc=mtc,
        formula=formula,
        ss_type=ss_type,
        ...)
    return(out)
}


.ANOVA<-setClass(
    "ANOVA",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='entity_stato',
        formula='entity',
        ss_type='enum',
        # OUTPUTS
        f_statistic='entity_stato',
        p_value='entity_stato',
        significant='entity'
    ),
    prototype = list(name='Analysis of Variance',
        description='ANOVA applied to each column of a DatasetExperiment.',
        type="univariate",
        predicted='p_value',
        stato_id="OBI:0200201",
        libraries='car',
        .params=c('alpha','mtc','formula','ss_type'),
        .outputs=c('f_statistic','p_value','significant'),

        alpha=ents$alpha,
        mtc=ents$mtc,
        formula=ents$formula,

        ss_type=enum(name='ANOVA type',
            description='I, II or [III]. The type of sums of squares to use. For balanced designs all types gives the same result.',
            value='III',
            type='character',
            allowed=c('I','II','III')
        ),

        f_statistic=ents$f_statistic,
        p_value=ents$p_value,
        significant=ents$significant
    )
)


#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("ANOVA",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        var_names=all.vars(M$formula)

        # attempt to detect within factors
        within=which(var_names %in% all.names(M$formula)[which('Error'== all.names(M$formula))+2])

        if (length(within)>0) {
            var_names_ex=var_names[-within]
        } else {
            var_names_ex=var_names
        }

        y=D$sample_meta[var_names[2:length(var_names)]]

        # set the contrasts
        O=options('contrasts') # keep the old ones
        options(contrasts = c("contr.sum","contr.poly"))

        output=apply(X,2,function(x) {
            temp=y
            temp[[var_names[1]]]=x

            # check number levels
            temp2=na.omit(temp)
            s=sapply(var_names_ex[2:length(var_names)],function(x) summary(temp2[[x]]))
            n=nrow(temp2)

            # check for alias columns
            dona=FALSE
            if (all(unlist(s)>2)) { # check we have enough levels
                temp3=temp[,var_names_ex] # ignore within factors
                al=alias(M$formula,temp3) # check we have independent columns
                if ('Complete' %in% names(al)) {
                    dona=TRUE
                }
            } else {
                dona=TRUE
            }

            # check for enough samples
            temp3=temp
            temp3[[var_names[1]]]=rnorm(nrow(y))
            LM=lm(formula=M$formula,data=temp3)
            p=nrow(summary(LM)$coefficients)
            if (n<(p+1)) {
                dona=TRUE
            }

            if (dona) {
                # missing values have probably prevented one or more combinations of levels being present
                # use some fake data to generate the output table then replace all the values with NA
                temp[[var_names[1]]]=rnorm(nrow(y))
                LM=lm(formula=M$formula,data=temp)
                A=car::Anova(LM,type=M$ss_type)
                A[!is.na(A)]=NA
                return(A)
            }

            LM=lm(formula=M$formula,data=temp)
            A=car::Anova(LM,type=M$ss_type)
            return(A)
        })

        f_statistic=sapply(output,function(x){
            x$`F value`
        })
        f_statistic=as.data.frame(t(f_statistic))
        colnames(f_statistic)=rownames(output[[1]])
        f_statistic=f_statistic[,2:(ncol(f_statistic)-1),drop=FALSE]


        p_value=sapply(output,function(x){
            x$`Pr(>F)`
        })
        p_value=as.data.frame(t(p_value))
        colnames(p_value)=rownames(output[[1]])
        p_value=p_value[,2:(ncol(p_value)-1),drop=FALSE]

        # fdr correct the p.values
        for (k in 1:ncol(p_value)) {
            p_value[, k]=p.adjust(p_value[ ,k],M$mtc)
        }

        # populate the object
        M$f_statistic=f_statistic
        M$p_value=p_value
        M$significant=as.data.frame(p_value<M$alpha)

        # reset the contrasts
        options(O)

        return(M)
    }
)









