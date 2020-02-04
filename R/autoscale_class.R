#' Autoscale
#'
#' Autoscaling centres the columns of the data in a DatasetExperiment object and divides by the standard deviation.
#'
#' @return A STRUCT model object with methods for autoscaling.
#'
#' @examples
#' D = iris_DatasetExperiment()
#' M = autoscale()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export autoscale
autoscale = function(...) {
    out=struct::new_struct('autoscale',...)
    return(out)
}

.autoscale<-setClass(
    "autoscale",
    contains='model',
    slots=c(
        autoscaled='DatasetExperiment',
        mean='numeric',
        sd='numeric'
    ),
    prototype = list(name='Autoscaling',
        type="preprocessing",
        predicted='autoscaled',
        .outputs=c('autoscaled','mean','sd')
    )
)

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("autoscale",'DatasetExperiment'),
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

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("autoscale",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        Xc=ascale(X,output_value(M,'mean'),output_value(M,'sd'))
        D$data=as.data.frame(Xc)
        D$name=c(D$name,'The data has been autoscaled')
        output_value(M,'autoscaled')=D
        return(M)
    }
)

ascale<- function(x,m,s) {
    # http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
    return( (x - rep(m, rep.int(nrow(x), ncol(x)))) / rep(s, rep.int(nrow(x), ncol(x))) )
}
