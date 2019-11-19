#' Coefficient of determination class
#'
#' Coefficient of determination (r-squared).
#' @export r_squared
#' @examples
#' MET = r_squared()
#'
r_squared<-setClass(
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

        M=matrix(colMeans(Y),nrow=1)
        O=matrix(1,nrow=nrow(Y),ncol=1)
        M=O %*% M

        SSR  = sum(sum((Yhat-M)^2))
        SSE  = sum(sum((Y-Yhat)^2))
        SSTO = sum(sum((Y-M)^2))

        R2=1-(SSE/SSTO)

        obj@value=R2

        return(obj)
    }
)


