#' Pareto scaling
#'
#' Pareto scaling centres the columns of the data in a DatasetExperiment object and divides by the square root of the standard deviation.
#'
#' @return A STRUCT model object with methods for pareto scaling.
#'
#' @examples
#' D = iris_DatasetExperiment()
#' M = pareto_scale()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#'
#' @param ... slots and values for the new object
#' @return struct object
#' @export pareto_scale
pareto_scale = function(...) {
    out=.pareto_scale()
    out=struct::new_struct(out,...)
    return(out)
}


.pareto_scale<-setClass(
    "pareto_scale",
    contains='model',
    slots=c(
        scaled='DatasetExperiment',
        mean='numeric',
        sd='numeric'
    ),
    prototype = list(name='Pareto scaling',
        type="preprocessing",
        predicted='scaled'
    )
)

#' @param ... slots and values for the new object
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("pareto_scale",'DatasetExperiment'),
    definition=function(M,D)
    {
        # column means
        X=D$data
        m=colMeans(X)
        output_value(M,'mean')=m
        s=apply(X, 2, sd)
        output_value(M,'sd')=s
        return(M)
    }
)

#' @param ... slots and values for the new object
#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("pareto_scale",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        Xc=pscale(X,output_value(M,'mean'),output_value(M,'sd'))
        D$data=as.data.frame(Xc)
        D$name=c(D$name,'The data has been autoscaled')
        output_value(M,'scaled')=D
        return(M)
    }
)

pscale<- function(x,m,s) {
    # http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
    return( (x - rep(m, rep.int(nrow(x), ncol(x)))) / rep(sqrt(s), rep.int(nrow(x), ncol(x))) )
}
