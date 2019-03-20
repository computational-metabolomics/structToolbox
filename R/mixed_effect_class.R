#' Mixed Effects model class
#'
#' Mixed Effects model class. Applies RE model for all features in a dataset
#'
#' @import struct
#' @import stats
#' @import lmerTest
#' @import emmeans
#' @export mixed_effect
mixed_effect<-setClass(
    "mixed_effect",
    contains=c('method','stato','ANOVA'), # inherits ANOVA
    prototype = list(name='Mixed effects model',
        description='Mixed effects model applied to each column of a dataset.',
        type="univariate",
        predicted='p_value',
        stato.id="STATO:0000189"
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("mixed_effect",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        var_names=all.vars(M$formula)

        lmer_formula=aov2lmer(M$formula)

        # attempt to detect within factors
        within=which(var_names %in% all.names(M$formula)[which('Error'== all.names(M$formula))+2])

        if (length(within)>0) {
            var_names_ex=var_names[-within]
        } else {
            var_names_ex=var_names
        }

        y=dataset.sample_meta(D)[var_names[2:length(var_names)]]

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
                al=alias(params$formula,temp3) # check we have independent columns
                if ('Complete' %in% names(al)) {
                    dona=TRUE
                }
            } else {
                dona=TRUE
            }

            # check for enough samples
            temp3=temp
            temp3[[var_names[1]]]=rnorm(nrow(y))
            LM=lmer(formula=lmer_formula,data=temp3)
            p=nrow(summary(LM)$coefficients)
            if (n<(p+1)) {
                dona=TRUE
            }

            if (dona) {
                # missing values have probably prevented one or more combinations of levels being present
                # use some fake data to generate the output table then replace all the values with NA
                temp[[var_names[1]]]=rnorm(nrow(y))
                LM=lmer(formula=lmer_formula,data=temp)
                A=Anova(LM,type=M$type)
                A[!is.na(A)]=NA
                return(A)
            }

            LM=lmer(formula=lmer_formula,data=temp)
            A=Anova(LM,type=M$type)
            return(A)
        })

        f_statistic=sapply(output,function(x){
            x$`F value`
        })
        f_statistic=as.data.frame(t(f_statistic))
        colnames(f_statistic)=rownames(output[[1]])
        f_statistic=f_statistic[,var_names[2:length(var_names)],drop=FALSE]


        p_value=sapply(output,function(x){
            x$`Pr(>F)`
        })
        p_value=as.data.frame(t(p_value))
        colnames(p_value)=rownames(output[[1]])
        p_value=p_value[,var_names[2:length(var_names)],drop=FALSE]

        # fdr correct the p.values
        for (k in 1:ncol(p_value)) {
            p_value[, k]=p.adjust(p_value[ ,k],M$mtc)
        }

        # populate the object
        M$f_statistic=f_statistic
        M$p_value=p_value
        M$significant=p_value<M$alpha

        return(M)
    }
)


aov2lmer = function(f) {
    # convert the formula to text
    txt=paste(f[2],f[3],sep='~')
    # split at Error
    txt=strsplit(txt,' + Error',fixed=TRUE)[[1]]
    if (length(txt)>3) {
        error('aov2lmr: Too many Error terms')
    }
    # parse the Error term
    pattern="[()*/]"
    s=strsplit(txt[[2]],pattern)[[1]]
    s=s[nchar(s)>0]
    # assume first one is pairing factor
    t1=paste0('(1|',s[1],')')
    tn=paste('(1|',s[2:length(s)],':',s[1],')',collapse='+')
    fnew=paste(txt[1],t1,tn,sep='+')
    fnew=as.formula(fnew,env=globalenv())
    return(fnew)
}






