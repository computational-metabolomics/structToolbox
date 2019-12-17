#' Coefficient of determination class
#'
#' Coefficient of determination (r-squared).
#' @export r_squared
#' @examples
#' MET = r_squared()
#'
r_squared = function(...) {
    out=.r_squared()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.r_squared<-setClass(
    "r_squared",
    contains='metric',
    prototype = list(name='r squared',
        type="regression"
    )
)

#' @export
#' @template calculate
setMethod(f="calculate",
    signature=c('r_squared'),
    definition=function(obj,Y,Yhat)
    {
        SSR  = sum((Yhat-mean(Y))^2)
        SSE  = sum((Y-Yhat)^2)
        SSTO = sum((Y-mean(Y))^2)

        R2=1-(SSE/SSTO)

        obj@value=R2

        return(obj)
    }
)


