#' Classical Least Squares regression
#'
#' Classical least squares, where y is the response and X is the design matrix,
#' applied to each feature individually. Here the response is taken from the
#' data matrix and the design matrix is the taken from the specified sample meta
#' data column.
#'
#' @import struct
#'
#' @templateVar paramNames c('alpha','mtc','factor_names')
#' @template common_params
#'
#' @param intercept [TRUE] or FALSE to include an intercept term in the fit
#'
#' @return A STRUCT method object with functions for applying classical least squares
#'
#' @examples
#' D = iris_dataset()
#' M = classical_lsq(factor_names = 'Species')
#' M = model.apply(M,D)
#'
#' @export classical_lsq

classical_lsq<-setClass(
    "classical_lsq",
    contains='model',
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_names='entity',
        params.intercept='entity',
        # OUTPUTS
        outputs.coefficients='entity',
        outputs.p_value='entity.stato',
        outputs.significant='entity',
        outputs.r_squared='entity',
        outputs.adj_r_squared='entity'
    ),
    prototype = list(name='Univariate Classical Least Squares Regression',
        description='classical least squares, where y is the response and x is the design matrix, applied to each feature individually.',
        type="univariate",
        predicted='p_value',

        params.intercept=entity(name='Include intercept',
            type='logical',
            description='TRUE or FALSE to include the intercept term when fitting the model.',
            value=TRUE
        ),

        params.factor_names=ents$factor_names,

        params.alpha=ents$alpha,

        params.mtc=ents$mtc,

        outputs.coefficients=entity(name='Regression coefficients',
            type='data.frame',
            description='The regression coefficients for each model.'
        ),
        outputs.p_value=ents$p_value,
        outputs.significant=ents$significant,
        outputs.r_squared=entity(name='R Squared',
            description='The value of R Squared for the fitted model.',
            type='data.frame'
        ),
        outputs.adj_r_squared=entity(name='Adjusted R Squared',
            description='The value ofAdjusted  R Squared for the fitted model.',
            type='data.frame'
        )
    )
)

#' @export
#' @template method_apply
setMethod(f="model.apply",
    signature=c("classical_lsq",'dataset'),
    definition=function(M,D)
    {
        # for classical least squares X is the design matrix, or sample_meta in our case
        X = D$sample_meta

        #if (is(M$factor_names,'character')) { # if a character vector then apply to all features.
        # get columns requested for fit
        #  X = X[ ,M$factor_names,drop=FALSE]
        #}
        # y  is the response, or in our case the data matrix
        y = D$data
        # assume X and y are already scaled appropriately in a previous step of the workflow


        fcn=function(n) {

            # use the linear model class
            if (M$intercept) {
                LM=linear_model(formula=y~1+.)
            } else {
                LM=linear_model(formula=y~0+.)
            }

            # create a dataset object we can use with the linear model class
            temp_y=data.frame(y=y[,n])
            if (is(M$factor_names,'character')) {
                X=X[,M$factor_names,drop=FALSE]

            } else { # assume list
                X=X[,M$factor_names[[n]],drop=FALSE]
            }
            Dlm=dataset(data=temp_y,sample_meta=X)

            LM=model.train(LM,Dlm)

            S=summary(LM$lm)

            return(S)
        }

        out=apply(as.matrix(1:ncol(y)),MARGIN=1,FUN=fcn)

        # reorganise outputs

        a=lapply(out,FUN=function(x) {rownames(coefficients(x))})
        u=unique(unlist(a))
        m=matrix(NA,nrow=ncol(y),ncol=length(u))
        df=data.frame(m)
        colnames(df)=u
        rownames(df)=colnames(y)
        df2=df
        for (i in 1:length(out))
        {
            c=data.frame(coefficients(out[[i]]),stringsAsFactors =FALSE,check.names = FALSE,check.rows=FALSE)
            n=rownames(c) # names of columns
            for (k in 1:length(n)) {
                w=which(colnames(df)==n[k])
                df[i,w]=c$Estimate[k]
                df2[i,w]=c$`Pr(>|t|)`[k]
            }
        }

        M$coefficients=df

        M$p_value=df2

        df3=data.frame('r_squared'=unlist(lapply(out,function(x){return(x$r.squared)})))
        df4=data.frame('adj_r_squared'=unlist(lapply(out,function(x){return(x$adj.r.squared)})))
        rownames(df3)=rownames(df)
        rownames(df4)=rownames(df)
        M$r_squared=df3
        M$adj_r_squared=df4

        # fdr correct the p-values
        M$p_value=as.data.frame(apply(M$p_value,2,FUN=function(x) {p.adjust(x,M$mtc)}))

        M$significant=as.data.frame(M$p_value<M$alpha)

        return(M)
    }
)









