#' @eval get_description('r_squared')
#' @export r_squared
#' @examples
#' MET = r_squared()
#'
r_squared = function(...) {
    out=struct::new_struct('r_squared',...)
    return(out)
}


.r_squared<-setClass(
    "r_squared",
    contains=c('metric'),
    prototype = list(
        name='Coefficient of determination (R-squared)',
        description=paste0(
            'R-squared is a metric used to assess the goodness of fit for ',
            'regression models. It measures how much variance of one variable ',
            'can be explained by another variable.'
        ),
        type="regression",
        ontology='STATO:0000564'
    )
)

#' @export
#' @template calculate
setMethod(f="calculate",
    signature=c('r_squared'),
    definition=function(obj,Y,Yhat) {

        # calculate R2
        SSR  = sum((Yhat-mean(Y))^2)
        SSE  = sum((Y-Yhat)^2)
        SSTO = sum((Y-mean(Y))^2)

        R2=1-(SSE/SSTO)

        obj@value=R2

        return(obj)
    }
)


