#'linear model class
#'
#' wrapper for R lm.
#'
#' @import struct
#' @export linear_model
#' @examples
#' M = linear_model()
linear_model<-setClass(
    "linear_model",
    contains='model',
    slots=c(
        # INPUTS
        params.formula='entity',
        params.na_action='enum',
        params.contrasts='entity',

        # OUTPUTS
        outputs.lm='entity',
        outputs.coefficients='entity',
        outputs.residuals='entity',
        outputs.fitted_values='entity',
        outputs.predicted_values='entity'

    ),
    prototype = list(name='Linear Model',
        description='Used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance.',
        type="regression",
        predicted='predicted_values',

        params.formula=entity(name='Model Formula',
            description='Compact symbolic form of the equation to be fitted using a linear model.',
            value=y~x,
            type='formula',
            max_length=Inf
        ),
        params.na_action=enum(name='NA Action',
            description='The action to be taken when encoutering NA',
            value='na.omit',
            type='character',
            list=c('na.omit','na.fail','na.exclude','na.pass')
        ),
        params.contrasts=entity(name='Contrasts',
            description='The contrasts associated with a factor. If NULL then the default contrasts are used.',
            type='list'
        ),

        outputs.lm=entity(name='Linear model object',
            description='The lm object for this model.',
            type='lm'
        ),
        outputs.coefficients=entity(name='Model coefficients',
            description='The coefficients for the fitted model.',
            type='numeric'
        ),
        outputs.residuals=entity(name='Residuals',
            description='The residuals for the fitted model.',
            type='numeric'
        ),
        outputs.fitted_values=entity(name='Fitted values',
            description='The fitted values for the data used to train the model.',
            type='numeric'
        ),
        outputs.predicted_values=entity(name='Predicted values',
            description='The predicted values for new data using the fitted model.',
            type='numeric'
        )
    )
)

#' @export
setMethod(f="model.train",
    signature=c("linear_model",'dataset'),
    definition=function(M,D)
    {
        X=cbind(D$data,D$sample_meta)
        if (is.null(M$contrasts)) {
            M$lm=lm(formula = M$formula, na.action = M$na_action,data=X) # default contrasts
        } else {
            M$lm=lm(formula = M$formula, na.action = M$na_action, contrasts = M$contrasts,data=X)
        }
        M$coefficients=coefficients(M$lm)
        M$residuals=residuals(M$lm)
        M$fitted_values=fitted(M$lm)
        return(M)
    }
)

#' @export
setMethod(f="model.predict",
    signature=c("linear_model",'dataset'),
    definition=function(M,D)
    {
        X=cbind(D$data,D$sample_meta)
        M$predicted_values(predict(M$lm,newdata = X))
        return(M)
    }
)







