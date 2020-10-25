#' @eval get_description('linear_model')
#' @import struct
#' @export linear_model
#' @examples
#' D = iris_DatasetExperiment()
#' M = linear_model(formula = y~Species)
#'
linear_model = function(formula,na_action='na.omit',contrasts=list(),...) {
    out=struct::new_struct('linear_model',
        formula=formula,
        na_action=na_action,
        contrasts=contrasts,
        ...)
    return(out)
}


.linear_model<-setClass(
    "linear_model",
    contains='model',
    slots=c(
        # INPUTS
        formula='entity',
        na_action='enum',
        contrasts='entity',

        # OUTPUTS
        lm='entity',
        coefficients='entity',
        residuals='entity',
        fitted_values='entity',
        predicted_values='entity',
        r_squared='entity',
        adj_r_squared='entity'

    ),
    prototype = list(name='Linear model',
        description=paste0(
            'Linear models can be used to carry out ',
            'regression, single stratum analysis of variance and analysis ',
            'of covariance.'),
        type="regression",
        predicted='predicted_values',
        .params=c('formula','na_action','contrasts'),
        .outputs=c('lm','coefficients','residuals','fitted_values','predicted_values','r_squared','adj_r_squared'),
        libraries='stats',
        formula=ents$formula,
        na_action=enum(name='NA action',
            description=c(
                'na.omit' = 'Incomplete cases are removed.',
                'na.fail' = 'An error is thrown if NA are present.',
                'na.exclude'='Incomplete cases are removed, and the output result is padded to the correct size using NA.',
                'na.pass' = 'Does not apply a linear model if NA are present.'
            ),
            value='na.omit',
            type='character',
            allowed=c('na.omit','na.fail','na.exclude','na.pass')
        ),
        contrasts=entity(name='Contrasts',
            description='The contrasts associated with a factor.',
            type='list'
        ),

        lm=entity(name='Linear model object',
            description='The lm object for this model_',
            type='lm'
        ),
        coefficients=entity(name='Model coefficients',
            description='The coefficients for the fitted model_',
            type='numeric'
        ),
        residuals=entity(name='Residuals',
            description='The residuals for the fitted model_',
            type='numeric'
        ),
        fitted_values=entity(name='Fitted values',
            description='The fitted values for the data used to train the model_',
            type='numeric'
        ),
        predicted_values=entity(name='Predicted values',
            description='The predicted values for new data using the fitted model_',
            type='numeric'
        ),
        r_squared=entity(name='R Squared',
            description='The value of R Squared for the fitted model_',
            type='numeric'
        ),
        adj_r_squared=entity(name='Adjusted R Squared',
            description='The value ofAdjusted  R Squared for the fitted model_',
            type='numeric'
        )
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("linear_model",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=cbind(D$data,D$sample_meta)
        if (length(M$contrasts)==0) {
            M$lm=lm(formula = M$formula, na.action = M$na_action,data=X) # default contrasts
        } else {
            M$lm=lm(formula = M$formula, na.action = M$na_action, contrasts = M$contrasts,data=X)
        }
        M$coefficients=coefficients(M$lm)
        M$residuals=residuals(M$lm)
        M$fitted_values=fitted(M$lm)

        M$r_squared=summary(M$lm)$r.squared
        M$adj_r_squared=summary(M$lm)$adj.r.squared

        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("linear_model",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=cbind(D$data,D$sample_meta)
        M$predicted_values(predict(M$lm,newdata = X))
        return(M)
    }
)







