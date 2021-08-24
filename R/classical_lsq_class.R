#' @eval get_description('classical_lsq')
#' @import struct
#' @examples
#' D = iris_DatasetExperiment()
#' M = classical_lsq(factor_names = 'Species')
#' M = model_apply(M,D)
#' @export classical_lsq

classical_lsq = function(alpha=0.05,mtc='fdr',factor_names,intercept=TRUE,...) {

    out=struct::new_struct('classical_lsq',
        alpha = alpha,
        mtc = mtc,
        factor_names = factor_names,
        intercept = intercept,
        ...)

    return(out)
}


.classical_lsq<-setClass(
    "classical_lsq",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity',
        mtc='enum',
        factor_names='entity',
        intercept='entity',
        # OUTPUTS
        coefficients='entity',
        p_value='entity',
        significant='entity',
        r_squared='entity',
        adj_r_squared='entity'
    ),
    prototype = list(
        name='Univariate Classical Least Squares Regression',
        description=paste0(
            'In univariate classical least squares regression a line is ',
            'fitted between each feature/variable and a response variable. ',
            'The fitted line minimises the sum of squared differences ',
            'between the true response and the predicted response. ',
            'The coefficients (offset, gradient) of the fit can be tested ',
            'for significance.'
        ),
        type="univariate",
        ontology='STATO:0000370',
        predicted='p_value',
        .params=c('alpha','mtc','factor_names','intercept'),
        .outputs=c('coefficients','p_value','significant',
            'r_squared','adj_r_squared'),

        intercept=entity(name='Model intercept',
            type='logical',
            description=c(
                'TRUE' = 'An intercept term is included in the model.',
                'FALSE' = 'An intercept term is not included in the model.'),
            value=TRUE
        ),

        factor_names=ents$factor_names,

        alpha=ents$alpha,

        mtc=ents$mtc,

        coefficients=entity(name='Regression coefficients',
            type='data.frame',
            description='The regression coefficients for each term in the model.',
            value=data.frame()
        ),
        p_value=ents$p_value,
        significant=ents$significant,
        r_squared=entity(name='R Squared',
            description='The value of R Squared for the fitted model',
            type='data.frame',
            value=data.frame()
        ),
        adj_r_squared=entity(name='Adjusted R Squared',
            description='The value ofAdjusted  R Squared for the fitted model',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("classical_lsq",'DatasetExperiment'),
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

            # create a DatasetExperiment object we can use with the linear model class
            temp_y=y[,n,drop=FALSE]
            if (is(M$factor_names,'character')) {
                X=X[,M$factor_names,drop=FALSE]

            } else { # assume list
                X=X[,M$factor_names[[n]],drop=FALSE]
            }
            colnames(temp_y)='y'
            Dlm=DatasetExperiment(data=temp_y,sample_meta=X,variable_meta=colnames(temp_y))

            LM=model_train(LM,Dlm)

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









