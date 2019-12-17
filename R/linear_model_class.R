#'linear model class
#'
#' wrapper for R lm.
#'
#' @import struct
#' @param ... slots and values for the new object 
#' @export linear_model
#' @examples
#' M = linear_model()
linear_model = function(...) {
    out=.linear_model()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.linear_model<-setClass(
    "linear_model",
    contains='model',
    slots=c(
        # INPUTS
        params_formula='entity',
        params_na_action='enum',
        params_contrasts='entity',

        # OUTPUTS
        outputs_lm='entity',
        outputs_coefficients='entity',
        outputs_residuals='entity',
        outputs_fitted_values='entity',
        outputs_predicted_values='entity',
        outputs_r_squared='entity',
        outputs_adj_r_squared='entity'

    ),
    prototype = list(name='Linear Model',
        description='Used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance.',
        type="regression",
        predicted='predicted_values',

        params_formula=entity(name='Model Formula',
            description='Compact symbolic form of the equation to be fitted using a linear model_',
            value=y~x,
            type='formula',
            max_length=Inf
        ),
        params_na_action=enum(name='NA Action',
            description='The action to be taken when encoutering NA',
            value='na.omit',
            type='character',
            list=c('na.omit','na.fail','na.exclude','na.pass')
        ),
        params_contrasts=entity(name='Contrasts',
            description='The contrasts associated with a factor. If zero length then the default contrasts are used.',
            type='list'
        ),

        outputs_lm=entity(name='Linear model object',
            description='The lm object for this model_',
            type='lm'
        ),
        outputs_coefficients=entity(name='Model coefficients',
            description='The coefficients for the fitted model_',
            type='numeric'
        ),
        outputs_residuals=entity(name='Residuals',
            description='The residuals for the fitted model_',
            type='numeric'
        ),
        outputs_fitted_values=entity(name='Fitted values',
            description='The fitted values for the data used to train the model_',
            type='numeric'
        ),
        outputs_predicted_values=entity(name='Predicted values',
            description='The predicted values for new data using the fitted model_',
            type='numeric'
        ),
        outputs_r_squared=entity(name='R Squared',
            description='The value of R Squared for the fitted model_',
            type='numeric'
        ),
        outputs_adj_r_squared=entity(name='Adjusted R Squared',
            description='The value ofAdjusted  R Squared for the fitted model_',
            type='numeric'
        )
    )
)

#' @param ... slots and values for the new object 
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

#' @param ... slots and values for the new object 
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







