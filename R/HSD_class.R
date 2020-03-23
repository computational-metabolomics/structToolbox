#' HSD model class
#'
#' Tukey's honest significant difference. Usually used in conjunction with ANOVA,
#' this model compares classes in a pairwise fashion to try to identify which groups
#' are different to the others (if any).
#'
#' @include anova_class.R
#' @param alpha The p-value threshold. Default alpha = 0.05.
#' @param mtc Multiple test correction method passed to \code{p.adjust}. Default mtc = 'fdr'.
#' @param formula The formula to use. See \code{lm} for details.
#' @param unbalanced TRUE or FALSE to apply correction for unbalanced designs. Default is FALSE.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export HSD
#' @examples
#' D = iris_DatasetExperiment()
#' M = HSD(formula=y~Species)
#' M = model_apply(M,D)
#'
HSD = function(alpha=0.05,mtc='fdr',formula,unbalanced=FALSE,...) {
    out=struct::new_struct('HSD',
        alpha=alpha,
        mtc=mtc,
        formula=formula,
        unbalanced=unbalanced,
        ...)
    return(out)
}


.HSD<-setClass(
    "HSD",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='entity_stato',
        formula='entity',
        unbalanced='entity',
        # OUTPUTS
        difference='data.frame',
        UCL='data.frame',
        LCL='data.frame',
        # means='data.frame',
        # sd='data.frame',
        # counts='data.frame',
        p_value='entity_stato',
        significant='entity'
    ),
    prototype = list(name='Tukey Honest Significant Difference',
        description='Tukey HSD post hoc test abblied to ANOVA object.',
        type="univariate",
        predicted='p_value',
        stato_id="STATO:0000187",
        libraries='agricolae',
        .params=c('alpha','mtc','formula','unbalanced'),
        .outputs=c('difference','UCL','LCL','p_value','significant'),

        alpha=entity_stato(name='Confidence level',
            stato_id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        mtc=entity_stato(name='Multiple Test Correction method',
            stato_id='OBI:0200089',
            value='none',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        ),
        unbalanced=entity(name='Unbalanced model',
            description='TRUE or [FALSE]. Apply correction for unbalanced designs.',
            value=FALSE,
            type='logical'
        ),
        formula=entity(name='Formula',
            value=y~x,
            type='formula',
            description='The formula to use'
        ),
        p_value=entity_stato(name='p value',
            stato_id='STATO:0000175',
            type='data.frame',
            description='the probability of observing the calculated t-statistic.'
        ),
        significant=entity(name='Significant features',
            #stato_id='STATO:0000069',
            type='data.frame',
            description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
        )
    )
)


#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("HSD",'DatasetExperiment'),
    definition=function(M,D) {
        X=D$data
        var_names=all.vars(M$formula)
        var_names_1=var_names[1]
        var_names=var_names[-1]
        y=D$sample_meta[var_names]

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
                A=agricolae::HSD.test(LM,x,group = FALSE)$comparison
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
        M$p_value=as.data.frame(apply(M$p_value,2,p.adjust,method=M$mtc))

        M$significant=as.data.frame(M$p_value<M$alpha)

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



