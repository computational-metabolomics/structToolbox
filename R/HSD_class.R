#' HSD model class
#'
#' HSD model class. Tukey's honest significant difference for ANOVA objects
#'
#' @import struct
#' @import stats
#' @import agricolae
#' @include anova_class.R
#' @export HSD
#' @examples
#' M = HSD()
HSD<-setClass(
    "HSD",
    contains=c('method','stato'),
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.formula='entity',
        params.unbalanced='entity',
        # OUTPUTS
        outputs.difference='data.frame',
        outputs.UCL='data.frame',
        outputs.LCL='data.frame',
        # outputs.means='data.frame',
        # outputs.sd='data.frame',
        # outputs.counts='data.frame',
        outputs.p_value='entity.stato',
        outputs.significant='entity'
    ),
    prototype = list(name='Tukey Honest Significant Difference',
        description='Tukey HSD post hoc test abblied to ANOVA object.',
        type="univariate",
        predicted='p_value',
        stato.id="STATO:0000187",

        params.alpha=entity.stato(name='Confidence level',
            stato.id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params.mtc=entity.stato(name='Multiple Test Correction method',
            stato.id='OBI:0200089',
            value='none',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        params.unbalanced=entity(name='Unbalanced model',
            description='TRUE or [FALSE]. Apply correction for unbalanced designs.',
            value=FALSE,
            type='logical'
        ),
        outputs.p_value=entity.stato(name='p value',
            stato.id='STATO:0000175',
            type='numeric',
            description='the probability of observing the calculated t-statistic.'
        ),
        outputs.significant=entity(name='Significant features',
            #stato.id='STATO:0000069',
            type='logical',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("HSD",'dataset'),
    definition=function(M,D) {
        X=dataset.data(D)
        var_names=all.vars(M$formula)
        var_names_1=var_names[1]
        var_names=var_names[-1]
        y=dataset.sample_meta(D)[var_names]

        # attempt to detect within factors
        within=which(all.names(M$formula) %in% all.names(M$formula)[which('Error'== all.names(M$formula))+2])
        if (length(within)>0) {
            var_names_ex=var_names[-within]
        } else {
            var_names_ex=var_names
        }

        FF=full_fact(var_names_ex)
        FF=apply(FF,1,function(x) var_names_ex[x==1])
        FF=FF[-1]

        # set the contrasts
        O=options('contrasts') # keep the old ones
        options(contrasts = c("contr.sum","contr.poly"))

        output=apply(X,2,function(x) {
            temp=y
            temp[[var_names_1]]=x

            # check for alias columns
            ALIAS=FALSE
            dona=FALSE

            # check n levels
            temp2=na.omit(temp)
            s=sapply(var_names,function(x) summary(temp2[[x]]))
            n=nrow(temp2)
            if (is.list(s)){
                s=unlist(s)
            }
            if (any(s<=2)) { # at least one level has 1 samples
                dona=TRUE
            }

            if (all(s>2)) {
                al=alias(M$formula,data=temp)
                if ('Complete' %in% names(al)) {
                    dona=TRUE
                }
            }

            # check for enough samples
            temp3=temp
            temp3[[var_names_1]]=rnorm(nrow(y))
            LM=lm(formula=M$formula,data=temp3)
            p=nrow(summary(LM)$coefficients)
            if (n<p) {
                dona=TRUE
            }

            if (dona) {
                # missing values have probably prevented one or more combinations of levels being present
                # use some fake data to generate the output table then replace all the values with NA
                temp[[var_names_1]]=rnorm(nrow(y))
                ALIAS=TRUE
            }
            LM=lm(formula=M$formula,data=temp)


            # for each combination of factors...
            out2=lapply(FF,function(x) {
                A=HSD.test(LM,x,group = FALSE)$comparison
                if (ALIAS) {
                    A[!is.na(A)]=NA # replace with NA if alias are present
                }
                return(A)
            })
            out2=do.call(rbind,out2)

            return(out2)
        })


        p_value=lapply(output,function(x) x$pvalue)
        p_value=do.call(rbind,p_value)
        M$p_value=data.frame(p_value)
        colnames(M$p_value)=rownames(output[[1]])

        # fdr correct
        M$p_value=apply(M$p_value,2,p.adjust,method=M$mtc)

        M$significant=M$p_value<M$alpha

        difference=lapply(output,function(x) x$difference)
        difference=do.call(rbind,difference)
        M$difference=data.frame(difference)
        colnames(M$difference)=rownames(output[[1]])

        UCL=lapply(output,function(x) x$UCL)
        UCL=do.call(rbind,UCL)
        M$UCL=data.frame(UCL)
        colnames(M$UCL)=rownames(output[[1]])

        LCL=lapply(output,function(x) x$LCL)
        LCL=do.call(rbind,LCL)
        M$LCL=data.frame(LCL)
        colnames(M$LCL)=rownames(output[[1]])

        options(O)

        return(M)
    }
)



full_fact = function(f,N=length(f),M=NULL) {
    if (N==length(f)) {
        M=matrix(c(0,1),nrow=2,ncol=1)
        if (N>1) {
            M=full_fact(f,N-1,M)
        }
    } else {
        M0=M1=M
        M0=cbind(M0,matrix(0,nrow=nrow(M)))
        M1=cbind(M1,matrix(1,nrow=nrow(M)))
        M=rbind(M0,M1)
        if (N>1) {
            M=full_fact(f,N-1,M)
        }
    }
    return(M[order(rowSums(M)),,drop=FALSE])
}



