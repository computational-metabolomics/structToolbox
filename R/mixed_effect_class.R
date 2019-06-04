#' Mixed Effects model class
#'
#' Mixed Effects model class. Applies RE model for all features in a dataset
#'
#' @import struct
#' @import stats
#' @import nlme
#' @import emmeans
#' @include anova_class.R
#' @export mixed_effect
mixed_effect<-setClass(
    "mixed_effect",
    contains=c('method','stato','ANOVA'), # inherits ANOVA
    prototype = list(name='Mixed effects model',
        description='Mixed effects model applied to each column of a dataset.',
        type="univariate",
        predicted='p_value',
        stato.id="STATO:0000189",
        params.type=enum(list=c('sequential','marginal'),
            value = 'marginal',
            name ='ANOVA Sum of Squares type',
            description = '"marginal" = Type III sum of squares, and "sequential" = Type II. Default is "marginal"',
            type='character')
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("mixed_effect",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        lmer_formula=aov2lme(M$formula)
        var_names=all.vars(M$formula)
        var_names_1=var_names[1]
        var_names=var_names[-1]
        y=dataset.sample_meta(D)[var_names]

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
            }, warning=function(w) {
                NA
            }, message=function(m) {
                NA
            }, error=function(e) {
                NA
            })

            if (!is.na(testlm[[1]])) {
                A=data.frame(anova(LM, type = M$type))
            } else {
                A=NA
            }


            return(A)
        })


        output=lapply(output,function(x) {
            if (length(x)!=length(output[[1]])) {
                x=output[[1]]*NA
                return(x)
            } else {
                return(x)
            }
        })
        f_statistic=sapply(output,function(x){
            x$`F.value`
        })
        if (!is.matrix(f_statistic)) {
            f_statistic=matrix(f_statistic,ncol=length(f_statistic))
        }
        f_statistic=data.frame(t(f_statistic))
        colnames(f_statistic)=rownames(output[[1]])
        f_statistic=f_statistic[,2:(ncol(f_statistic)),drop=FALSE]


        p_value=sapply(output,function(x){
            x$`p.value`
        })
        if (!is.matrix(p_value)) {
            p_value=matrix(p_value,ncol=length(p_value))
        }
        p_value=data.frame(t(p_value))
        colnames(p_value)=rownames(output[[1]])
        p_value=p_value[,2:(ncol(p_value)),drop=FALSE]

        # fdr correct the p.values
        for (k in 1:ncol(p_value)) {
            p_value[, k]=p.adjust(p_value[ ,k],M$mtc)
        }

        # populate the object
        M$f_statistic=f_statistic
        M$p_value=p_value
        M$significant=p_value<M$alpha

        # reset contrasts
        options(O)

        return(M)
    }
)


aov2lme = function(f) {
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
    s=trimws(s)
    s=s[nchar(s)>0]

    # assume first one is pairing factor
    out=list()
    if (length(s)==2) {# if we only have one additional factor then
        out$random=formula(paste0('~1 | ',s[1]),env=globalenv())
    } else {

        str=character(length(s)-1)
        for (k in 2:length(s)) {
            str[k-1]=paste0('pdIdent(~',s[k],'-1)')
        }
        str=paste0('list(',s[1],'=pdBlocked(list(~1,',paste(str,collapse=','),')))')
        out$random=eval(parse(text = str))
    }
    out$f=formula(txt[1],env=globalenv())
    return(out)
}




